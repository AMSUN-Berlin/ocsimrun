(*
 * Copyright (c) 2014, TU Berlin
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *   * Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   * Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *   * Neither the name of the TU Berlin nor the
 *     names of its contributors may be used to endorse or promote products
 *     derived from this software without specific prior written permission.

 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL TU Berlin BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 *)

open Core
open Monads
open Batteries

type experiment = {
  rtol : float ;
  atol : float ;
  start : float ;
  minstep: float;
  stop : float;
}

module type SimEngine = sig
    type simulation_state
    val compute_unknown : simulation_state -> unknown -> float

    type 'r state_trait = <get_sim_state : simulation_state option; set_sim_state : simulation_state option -> 'r; .. > as 'r
    constraint 'r = 'r FlatEvents.state_trait

    type ('r, 'a) sim_monad = 'r -> ('r * 'a)
    constraint 'r = 'r state_trait

    val init : experiment -> ('r, int) sim_monad
    val perform_step : float -> ('r, float) sim_monad
    val perform_step : float -> ('r, bool * float) sim_monad
    val sim_loop : experiment -> ('r, float) sim_monad
    val simulate : experiment -> ('r, float) sim_monad
    val simulation_state : ('r, simulation_state option) sim_monad
end

type result = Success | Error of int * string

module SundialsImpl : SimEngine = struct
  open Bigarray
  open FlatLayout
  open FlatEvents
  open Lens
  open Monads.ObjectStateMonad
       
  let fvector = Array1.create float64 c_layout 

  let last arr = arr.((Array.length arr) - 1)

  type numeric_state = {
    t : float ;
    yy : fvector ;
    yp : fvector ;
    res : fvector ;
  }

  type event_state = {
    e_t : float ;
    e_y : fvector ;
    e_yp : fvector ;
    e_gi : fvector ;
  }

  type simulation_state = {
    num_state : numeric_state;
    layout : layout ;
  }	     

  type 'r state_trait = <get_sim_state : simulation_state option; set_sim_state : simulation_state option -> 'r; .. > as 'r
  constraint 'r = 'r FlatEvents.state_trait

  type ('r, 'a) sim_monad = 'r -> ('r * 'a)
  constraint 'r = 'r state_trait

  let compute_unknown {layout;num_state} u =
    let _ = Printf.printf "unpack done\n%!" in
    let fu = layout.flatten_unk u in
    layout.compute_unk num_state.yy num_state.yp fu 


  let state = { get = (fun a -> a#get_sim_state) ; set = fun a b -> b#set_sim_state a }

  let simulation_state s = ( perform (
			       o <-- get state ;
			       return o;
			   ) ) s


  let rec handle_root gs i = if i < Array.length gs then (
			       match gs.(i) with
				 1 -> root_found i Gt &. handle_root gs (i+1)
			       | -1 -> root_found i Lt &. handle_root gs (i+1)
			       | 0 -> handle_root gs (i+1)
			     ) else return ()

  let perform_reinit {start;minstep} = perform (
					   sim <-- get state ;
					   es <-- FlatEvents.event_state ;
					   let layout = FlatEvents.layout es in 
					   (* check, if layout has changed *)

					   let _ = if layout.u_mark != sim.layout.u_mark or layout.e_mark != sim.layout.e_mark then
						     raise ( Failure("Structural changes not (yet) supported.") )
						   else () in
				 
					   return ()
					 )
	 
  let perform_step t  = perform ( 
				 sim <-- get state ;
				 event_dim <-- event_array_size ;

				 _ <-- reschedule sim.num_state.yy sim.num_state.yp ;

				 reinit_needed <-- effects_exist ;

				 _ <-- event_loop sim.num_state.yy sim.num_state.yp ;

				 return (reinit_needed, t +. 0.1)
			     )

  let init ({start; minstep} as exp) = 
    perform (
	es <-- FlatEvents.event_state ;
	let layout = FlatEvents.layout es in 

	let dim = layout.dimension in
	
	let id = fvector dim in
	let yy = fvector dim in
	let yp = fvector dim in
	let res = fvector dim in

	let _ = (
	  (* INIT HERE *)
	  yy.{0} <- start ;
	  yp.{0} <- 1.0 ) in
	
	(* Set remaining input parameters. *)
	let rtol = 0. in
	let atol = 1.0e-3 in


	unknowns <-- all_unknowns ;

	let set_id = function 
	  | Algebraic _ | LowState _  -> ()
	  | State(yy,yp) -> Printf.printf "id(%d %d / %d) <- 1.\n" yy yp dim ; id.{yp} <- 1.
	  | Derivative i -> id.{i} <- 1. in     

	let _ = id.{0} <- 1. ; Enum.iter (set_id % layout.flatten_unk) unknowns in

	let _ = Printf.printf "id: %s\n" (IO.to_string (Array.print Float.print) (Bigarray.Array1.to_array id)) in
	
	event_roots <-- event_roots ;

	let num_state = { t = start ; yy ; yp ; res } in

	let residual {t; yy; yp; res} = 
	  yy.{0} <- t ; 
	  layout.compute_res yy yp res 
	in

	let event_roots {e_t; e_y; e_yp; e_gi} =
	  e_y.{0} <- e_t;
	  event_roots e_y e_yp e_gi 
	in

	let sim = { num_state ; layout } in

	_ <-- put state sim  ;
	
	_ <-- schedule_clocks yy yp ;

	return 0
      )


  let rec sim_loop ({start; minstep; stop} as exp) = 
    perform (			     
	(reinit, t') <-- perform_step (start +. minstep) ;
	
	_ <-- if reinit then perform_reinit exp else return () ;

	if t' < stop then
	  sim_loop {exp with start=t'}
	else
	  return t'
      )
	    
  let simulate exp = init exp &. sim_loop exp
			 
end
