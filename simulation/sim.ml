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

open Unknowns
open Equations
open Events
open Monads
open Models
open Batteries

type experiment = {
  rtol : float ;
  atol : float ;
  start : float ;
  stop : float;
}

type result = Success | Error of int * string

module type ENGINE_IMPL = 
  sig
    val simulate : 'r mode -> experiment -> result
  end


module SundialsImpl = struct
  open Ida	   
  open Ida_utils
  open FlatLayout
  open FlatEvents
  open FlatDAE

  open Async.Std
		
  module IntMap = Map.Make(Int)

  let last arr = arr.((Array.length arr) - 1)
		    	     
  let simulate m exp = 
    let us =  m.unknowns 
    and evs = List.of_enum (IntMap.values m.events)
    and eqs = m.equations in
    
    Printf.printf "# Simulating %d equations and %d unknowns\n%!" (Enum.count (IntMap.enum eqs)) (Enum.count (UnknownSet.enum us)) ;

    let flatDAE = flatten_model us eqs evs in
    let dim = flatDAE.layout.dimension in

    (* the last entry in the last layout-row returns the size of the yy-vector *)
    Printf.printf "Dimension: %d\n" flatDAE.layout.dimension ;

    let yy = flatDAE.yy_vec in
    let yp = flatDAE.yp_vec in
    let id = fvector dim in
    let res = fvector dim in
    let num_state = { t = exp.start; yp = yp; yy = yy; res = res } in
    let eval_unknown u = compute_fu yy yp (flatten_unknown flatDAE.layout u) in
    let eval_eq = compute_feq yy yp in

    let prestart u = 0. in

    let residual {t;yy;yp;res} = residual flatDAE.layout.equalities flatDAE.flat_equations yy yp res in

    (* first, set all unknowns to 0 *)
    let pre_event_state = flatDAE.flat_event_state in

    let (start_effects, event_state, _) = reschedule pre_event_state exp.start [] in

    (* handle all pre-init events *)
    let handle_start_effect = function Unknown(u, v) -> update_fu yy yp v (flatten_unknown flatDAE.layout u) | _ -> () in
    List.iter (fun gen -> let effs = gen eval_unknown in List.iter handle_start_effect effs) start_effects;

    (* update event memory, since start events might change it *)
    update_mem event_state eval_eq;

    (* INIT HERE *)
    yy.{0} <- exp.start ;
    yp.{0} <- 1.0 ;

    let event_roots {e_t;e_y;e_yp;e_gi} = 
      let eval_eq = compute_feq e_y e_yp in

      let relations = relations event_state in
      for i = 0 to (Array.length relations) - 1 do	
	e_gi . {i} <- eval_eq relations.(i).flat_relation
      done ;
      0
    in

    Printf.printf "# Got %d continuous-time relations\n" (Array.length (relations event_state));

    (* Set remaining input parameters. *)
    let rtol = 0. in
    let atol = 1.0e-3 in

    (* Call IDACreate and IDAMalloc to initialize solution *)
    let ida = ida_create () in
    
    let set_eq_id {yy_index;yp_index} = id.{yp_index} <- 1. in
    let set_id = function 
      | Algebraic _ | LowState _ | State(_,_) -> ()
      | Derivative i -> id.{i} <- 1. in     
    let set_uk_id f_eq = Array.iter set_id (depends f_eq) in

    id.{0} <- 1. ; 
    Array.iter set_eq_id flatDAE.layout.equalities ;
    Array.iter set_uk_id flatDAE.flat_equations ;

    Printf.printf "id: %s\n" (IO.to_string (Array.print Float.print) (Bigarray.Array1.to_array id)) ;
    check_flag "IDASetId" (ida_set_id ida id) ;
    
    let event_dim = Array.length (relations event_state) in

    let ida_event_state = { e_t = 0.0; e_y = fvector dim ; e_yp = fvector dim ; e_gi = fvector event_dim } in
  
    let ida_ctxt = { residual = residual ; state = num_state ; event_roots ; event_state = ida_event_state } in
    
    check_flag "IDAInit" (ida_init ida ida_ctxt) ;

    check_flag "IDASStolerances" (ida_ss_tolerances ida rtol atol);

    check_flag "IDADense" (ida_dense ida dim) ;
 
    (* Call IDACalcIC to correct the initial values. *)
    let minstep = exp.stop /. 500. in

    Printf.printf "Initialization...\n%!";
    check_flag "IDACalcIC" (ida_calc_ic ida ida_ya_ydp_init (exp.start +. minstep)) ;

    let rootsfound = Array.create event_dim 0 in

    let apply_effect reinit = function 
      | Unknown (u, v) -> update_fu yy yp v (flatten_unknown flatDAE.layout u) ; true
      | Nothing -> reinit
      | _ -> reinit (* no other effects allowed for now *)
    in 
    
    let event_loop e_state gs = 
      List.fold_left (fun reinit -> (fun gen -> List.fold_left apply_effect reinit (gen eval_unknown))) false gs
    in

    let event_loop_start e_state ret t = 
      let handle_root gs i n = if n != 0 then (
				 Printf.printf "root: %d, sign: %d\n" i n ;
				 relation_fired e_state i n eval_eq gs
			       ) else
				 gs
      in
      let (gs', e_state', samples) = reschedule e_state t
						( if ret = ida_root_return then (
						    check_flag "IDAGetRootInfo" (ida_get_root_info ida rootsfound) ;
						    Array.fold_lefti handle_root [] rootsfound	 	 
						  ) else []
				       )
      in (e_state', event_loop e_state' gs', samples)
    in

    (* Loop over output times, call IDASolve. *)
    let rec sim_loop e_state step = if step.tret < exp.stop then (
				      let ret = ida_solve ida step ida_normal in
				      check_flag "IDASolve" ret ;
				      Printf.printf "Sim-time: %f\n%!" step.tret;
				    
				      let (e_state', reinit_needed, samples) = event_loop_start e_state ret step.tret in
				      if reinit_needed then ( 
					check_flag "IDAReInit" (ida_reinit ida step.tret ida_ctxt) ; 
					check_flag "IDACalcIC" (ida_calc_ic ida ida_ya_ydp_init (step.tret +. minstep))  					
				      ) ;
				    
				      update_mem e_state' eval_eq ;
				      
				      (next_state e_state' eval_unknown samples) >>= fun e_state'' -> sim_loop e_state'' { step with tout = step.tret +. minstep } 
				    ) else (
				      Printf.printf "Done at time: %f\n%!" step.tret;
				      Deferred.return Success
				    )
    in

    Printf.printf "# Starting sim-loop...\n%!" ;
    let step = { tout = minstep ; tret = exp.start } in
    sim_loop event_state step 

end
