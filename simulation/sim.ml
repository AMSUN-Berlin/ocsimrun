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

    type 'r state_trait = <get_sim_state : simulation_state option; set_sim_state : simulation_state option -> 'r; .. > as 'r
    constraint 'r = 'r FlatEvents.state_trait

    type ('r, 'a) sim_monad = 'r -> ('r * 'a)
    constraint 'r = 'r state_trait

    val compute_unknown : simulation_state -> unknown -> float
    val init : experiment -> ('r, int) sim_monad
    val perform_step : float -> ('r, float) sim_monad
    val perform_step : float -> ('r, bool * float) sim_monad
    val sim_loop : experiment -> ('r, float) sim_monad
    val simulate : experiment -> ('r, float) sim_monad
    val simulation_state : ('r, simulation_state option) sim_monad 
end

type result = Success | Error of int * string

module SundialsImpl : SimEngine = struct
  open Ida	   
  open Ida_utils
  open FlatLayout
  open FlatEvents
  open Lens
  open Monads.ObjectStateMonad

  let last arr = arr.((Array.length arr) - 1)

  type simulation_state = {
    ida : ida_solver;
    num_state : numeric_state;
    layout : layout ;
  }	     

  let compute_unknown {ida;layout;num_state} u = 
    let fu = layout.flatten_unk u in
    layout.compute_unk num_state.yy num_state.yp fu

  let update_unknown {ida;layout;num_state} u f = 
    let fu = layout.flatten_unk u in
    layout.update_unk num_state.yy num_state.yp f fu

  type 'r state_trait = <get_sim_state : simulation_state option; set_sim_state : simulation_state option -> 'r; .. > as 'r
  constraint 'r = 'r FlatEvents.state_trait

  type ('r, 'a) sim_monad = 'r -> ('r * 'a)
  constraint 'r = 'r state_trait

  let state = { get = (fun a -> a#get_sim_state) ; set = fun a b -> b#set_sim_state a }

  let simulation_state s = (perform ( 
				sim <-- get state ; 
				return sim 
			   ) ) s

  let get_sim_state s = ( perform (
			    o <-- get state ;
			    match o with Some(sim) -> return sim 
				       | None -> raise (Failure "No simulation state set-up!")
			)) s 

  let rec handle_root gs i = if i < Array.length gs then (
			       match gs.(i) with
				 1 -> root_found i Gt &. handle_root gs (i+1)
			       | -1 -> root_found i Lt &. handle_root gs (i+1)
			       | 0 -> handle_root gs (i+1)
			     ) else return ()

  let perform_reinit {start;minstep} = perform (
					   sim <-- get_sim_state ;					   
					   es <-- FlatEvents.event_state ;
					   let layout = FlatEvents.layout es in 
					   (* check, if layout has changed *)

					   let _ = if layout.u_mark != sim.layout.u_mark or layout.e_mark != sim.layout.e_mark then
						     raise ( Failure("Structural changes not (yet) supported.") )
						   else () in
				 
					   let _ = check_flag "IDAReInit" (ida_reinit sim.ida start (ida_get_ctxt sim.ida)) ; 
						   check_flag "IDACalcIC" (ida_calc_ic sim.ida ida_ya_ydp_init (start +. minstep)) in												
					   return ()
					 )
	 
  let perform_step t  = perform ( 
				 sim <-- get_sim_state ;
				 event_dim <-- event_array_size ;

				 let step = {tret = t ; tout = t} in

				 let ret = ida_solve sim.ida step ida_normal in
		     
				 let t' = compute_unknown sim time in

				 let _ = check_flag "IDASolve" ret ;
					 Printf.printf "Sim-time: %f ret: %f\n%!" t' step.tret 

				 in
				 
				 let _ = Printf.printf "yy: %s\nyp: %s\n" 
						       (IO.to_string (Array.print Float.print) (Bigarray.Array1.to_array sim.num_state.yy)) 
						       (IO.to_string (Array.print Float.print) (Bigarray.Array1.to_array sim.num_state.yp))
				 in

				 let rootsfound = Array.create event_dim 0 in

				 _ <-- (if ret = ida_root_return then 					 
					 let _ = check_flag "IDAGetRootInfo" (ida_get_root_info sim.ida rootsfound) in
					 handle_root rootsfound 0 ;
				       else
					 return () );				 				 

				 _ <-- reschedule sim.num_state.yy sim.num_state.yp ;

				 reinit_needed <-- effects_exist ;

				 _ <-- event_loop sim.num_state.yy sim.num_state.yp ;

				 return (reinit_needed, step.tout)
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

	(* Call IDACreate and IDAMalloc to initialize solution *)
	let ida = ida_create () in
	
	unknowns <-- all_unknowns ;

	let set_id = function 
	  | Algebraic _ | LowState _  -> ()
	  | State(yy,yp) -> id.{yp} <- 1.
	  | Derivative i -> id.{i} <- 1. in     

	let _ = id.{0} <- 1. ; Enum.iter (set_id % layout.flatten_unk) unknowns in

	let _ = Printf.printf "id: %s\n" (IO.to_string (Array.print Float.print) (Bigarray.Array1.to_array id)) in
	
	let _ = check_flag "IDASetId" (ida_set_id ida id) in
	
	event_dim <-- event_array_size ; 
	
	let ida_event_state = { e_t = 0.0; e_y = fvector dim ; e_yp = fvector dim ; e_gi = fvector event_dim } in
	
	event_roots <-- event_roots ;

	let num_state = { t = start ; Ida.yy = yy ; yp ; res } in

	let residual {t; yy; yp; res} = 
	  yy.{0} <- t ; 
	  layout.compute_res yy yp res 
	in

	let event_roots {e_t; e_y; e_yp; e_gi} =
	  e_y.{0} <- e_t;
	  event_roots e_y e_yp e_gi 
	in

	let sim = { ida ; num_state ; layout } in

	(* apply the start values *)
	svs <-- start_values ;

	let _ = Enum.iter (fun (u,f) -> Printf.printf "%s starts at %f\n" (string_of_unknown u) f ; update_unknown sim u f) svs in

	let ida_ctxt = { residual ; state = num_state ; event_roots ; event_state = ida_event_state } ;		     
	in 
	let _ =
	  check_flag "IDAInit" (ida_init ida ida_ctxt) ;

	  check_flag "IDASStolerances" (ida_ss_tolerances ida rtol atol);

	  check_flag "IDADense" (ida_dense ida dim) ;
	in

	(* Call IDACalcIC to correct the initial values. *)
	let _ = 
	  Printf.printf "Initialization...\n%!";
	  check_flag "IDACalcIC" (ida_calc_ic ida ida_ya_ydp_init (start +. minstep)) ;
	in

	let _ = Printf.printf "yy: %s\nyp: %s\n" 
			      (IO.to_string (Array.print Float.print) (Bigarray.Array1.to_array yy)) 
			      (IO.to_string (Array.print Float.print) (Bigarray.Array1.to_array yp))
	in

	_ <-- put state (Some sim)  ;

	_ <-- schedule_clocks yy yp ;

	_ <-- reschedule sim.num_state.yy sim.num_state.yp ;
	
	reinit_needed <-- effects_exist ;

	_ <-- event_loop sim.num_state.yy sim.num_state.yp ;

	_ <-- if reinit_needed then
		perform_reinit exp 
	      else return () ;

	_ <-- set_sim_interface { value_of = compute_unknown sim ; set_value = update_unknown sim } ;
		

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
