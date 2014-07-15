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

(** 
This module provides a "flattening" of unknowns, events and equations 
into a form suitable for order-1 DAE solvers (i.e. it reduces any system
to an implicit order-1 DAE).
 *)

open Batteries
open Core
open Bigarray

type fvector = (float, float64_elt, c_layout) Array1.t

type layout = {
  dimension : int;
  compute_res : fvector -> fvector -> fvector -> int;
  compute_unk : unknown -> fvector -> fvector -> float;
  compute_eq : equation -> fvector -> fvector -> float;
  u_mark : int;
  e_mark : int;
}

open Monads.ObjectStateMonad

(** Distinguish between a yy or yp entry *)
type flat_unknown = LowState of int | State of int * int | Derivative of int | Algebraic of int

let rec fill_states u hd (dn, sn, fm) = function 
  (* only the 0-derivative is a plain flat state *)
  | 0 -> fill_states u hd (dn, sn+1, UnknownMap.add {u_idx=u; u_der=0} (LowState sn) fm) 1
		     
  (* only the highest derivative is a plain flat derivative *) 
  | n when n = hd -> (dn+1, sn, UnknownMap.add {u_idx=u; u_der=n} (Derivative dn) fm)		       

  (* everything else is both *)
  | n -> (fill_states u hd (dn+1, sn+1, (UnknownMap.add {u_idx=u; u_der=n} (State(dn, sn)) fm) ) (n+1))

(** 
  flatten a row of unknown starting at derivative index @dn and state index @sn 
  @return the new derivative index, new state index and the flat unknowns
*)
let flatten_unknown u (dn, sn, fm) = 
  if not (UnknownMap.mem u fm) then    
    perform (
	hd <-- der_order u;
	return ( match hd with 
		   (* if the highest derivative of row @u is 0, we have one algebraic var *)
		   Some(0) -> (dn, sn+1, UnknownMap.add {u_idx=u.u_idx;u_der=0} (Algebraic(sn)) fm)
                 (* otherwise, create all the states and derivatives *)						       
		 | Some n -> fill_states u.u_idx n (dn,sn,fm) 0
		 | None -> (dn, sn, fm)
	       )
      )
  else return (dn, sn, fm)

type flat_equation = Flat_Linear of (flat_unknown array) * (float array) * float  (** linear equation with constant coeffs *)

let compute_fu yy yp = function
  | Algebraic i -> yy.{i}
  | LowState i -> yy.{i}
  | State (i, _) -> yy.{i}
  | Derivative i -> yp.{i}

let compute_feq yy yp = function
    Flat_Linear(us, fs, c) -> 
    let calc s i f = f *. (compute_fu yy yp us.(i)) +. s in
    Array.fold_lefti calc c fs

let update_fu yy yp value = function
  | Algebraic i -> yy.{i} <- value
  | State (yy_index, yp_index) -> yy.{yy_index} <- value ; yp.{yp_index} <- value 
  | LowState i -> yy.{i} <- value
  | Derivative i -> yp.{i} <- value

let flatten_equation fm = function
  | Linear(us, cs, c) -> Flat_Linear(Array.map (fun u -> UnknownMap.find u fm) us, cs, c)


let residual equalities eqs yy yp res = 
  let compute_feq = compute_feq yy yp in

  (* the 0-th unknown is the time *)
  res.{0} <- yp.{0} -. 1. ;

  for i = 1 to Array.length eqs do
    (* Printf.printf "res[%d] <- %f\n" i (compute_feq eqs.(i-1)) ; *)
    res.{i} <- compute_feq eqs.(i-1)			  
  done ;

  let offset = Array.length eqs + 1 in
  let equality r (yy_index, yp_index) = 
    (* Printf.printf "yy[%d] = yp[%d]\n" yy_index yp_index ; *)
    res.{r+offset} <- yy.{yy_index} -. yp.{yp_index} in

  Array.iteri equality equalities ;
  
  (*
  Printf.printf "yy: %s\n" (IO.to_string (Array.print Float.print) (Bigarray.Array1.to_array yy)) ;
  Printf.printf "yp: %s\n" (IO.to_string (Array.print Float.print) (Bigarray.Array1.to_array yp)) ;
  Printf.printf "residual: %s\n%!" (IO.to_string (Array.print Float.print) (Bigarray.Array1.to_array res));  *)
  
  0

let collect_equalities fus = [? (yy_index, yp_index) | State(yy_index, yp_index) <- UnknownMap.values fus ?]

let flatten s = ( perform (
		      (* get the structural operations counter for validity check *)
		      u_mark <-- unknown_mark ;
		      e_mark <-- equation_mark ;
		      
		      us <-- all_unknowns ;
                      (dn,sn,flat_us) <-- (fold_enum flatten_unknown (0,0,UnknownMap.empty) us);
		      
		      dimension <-- current_dimension ;
		      
		      let compute_unk u yy yp = compute_fu yy yp (UnknownMap.find u flat_us) in
		      let compute_eq  e yy yp = compute_feq yy yp (flatten_equation flat_us e) in
		      let equalities = Array.of_enum (collect_equalities flat_us) in
		      
		      equations <-- all_equations ;

		      let feqs = Array.map (flatten_equation flat_us) (Array.of_enum equations) in

		      let compute_res = residual equalities feqs in

		      return {dimension ; compute_res ; compute_eq ; compute_unk ; u_mark ; e_mark}
		) ) s 

let is_valid layout = perform (
			  u_mark <-- unknown_mark ;
			  e_mark <-- equation_mark ;
			  return (u_mark = layout.u_mark && e_mark = layout.e_mark)
			)

let validate layout = perform (
			  v <-- is_valid layout;
			  l <-- if v then return layout else flatten ;
			  return l
			)
