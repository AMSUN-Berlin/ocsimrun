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

open Unknowns
open Equations
open Batteries
open Events

(** Distinguish between a yy or yp entry *)
type flat_unknown = State of int | Derivative of int

module IntMap = Map.Make(Int)

(* structural equality between an order-1 derivative and state variable *)
type equality = {
  yy_index : int ;
  yp_index : int ;
}

type layout = {
  dimension : int;
  of_unknown : flat_unknown UnknownMap.t;
  of_equation : int IntMap.t;
  equalities : equality array ;
}


let highest_der us eqs = 
  let collect {u_idx; u_der} max = IntMap.modify u_idx (Int.max u_der) max in

  let min_der = IntMap.of_enum (Enum.map (fun {u_idx;u_der} -> (u_idx, 0)) (UnknownSet.enum us)) in

  let from_equation i _ max = UnknownSet.fold collect (depends (IntMap.find i eqs)) max in

  IntMap.fold from_equation eqs (IntMap.add 0 1 min_der)

let layout us eqs = 
  let hd = highest_der us eqs in

  let reserve_unknown {u_idx;u_der} (count, equals, m) = 
    if UnknownMap.mem {u_idx;u_der} m then 
      (count, equals, m) 
    else
      let der = IntMap.find u_idx hd in

      (* add all unknowns from der=0 to max-1 to the state vector 
         and all states from der=1 to max-1 equal the corresponding
         derivative
       *)
      let add_alg (equals, m) u_der = 
	let yy_index = count + u_der in
	let yp_index = count + u_der - 1 in	
	( (if u_der > 0 then {yy_index;yp_index}::equals else equals), 
	  UnknownMap.add {u_idx; u_der} (State(yy_index)) m)
      in

      let (equals', states) = Enum.fold add_alg (equals, m) (0--^der) in
      (* der=max is the only value from the derivative vector *)
      (count + (Int.max 1 (der - 1)), equals', UnknownMap.add {u_idx; u_der=der} (Derivative (count + der - 1)) states)
  in

  (* simply fill from 0 to length *)
  let reserve_direct i _ (c, m) = 
    (c+1, IntMap.add i c m)
  in

  let (us, equals, of_unknown) = UnknownSet.fold reserve_unknown us (0, [], UnknownMap.empty) in
  let (_, of_equation) = IntMap.fold reserve_direct eqs (0,  IntMap.empty) in
  let equalities=Array.of_list equals in
  let dimension = 1 + IntMap.cardinal of_equation + Array.length equalities in

  { dimension ; of_unknown ; of_equation ; equalities }

type flat_equation = Flat_Linear of (flat_unknown array) * (float array) * float  (** linear equation with constant coeffs *)

let flatten_unknown layout u = UnknownMap.find u layout.of_unknown

let flatten_equation layout = function
  | Linear(us, cs, c) -> Flat_Linear(Array.map (flatten_unknown layout) us, cs, c)

let depends = function
    Flat_Linear (us, _, _) -> us

let update_fu yy yp value = function
  | State i -> yy.{i} <- value
  | Derivative i -> yp.{i} <- value

let compute_fu yy yp = function
  | State i -> yy.{i}
  | Derivative i -> yp.{i}

let compute_feq yy yp = function
    Flat_Linear(us, fs, c) -> 
    let calc s i f = f *. (compute_fu yy yp us.(i)) +. s in
    Array.fold_lefti calc c fs

let residual equalities eqs yy yp res = 
  let compute_feq = compute_feq yy yp in

  (* the 0-th unknown is the time *)
  res.{0} <- yp.{0} -. 1. ;

  for i = 1 to Array.length eqs do
    res.{i} <- compute_feq eqs.(i-1)			  
  done ;

  let offset = Array.length eqs + 1 in
  let equality r {yy_index; yp_index} = res.{r+offset} <- yy.{yy_index} -. yp.{yp_index} in

  Array.iteri equality equalities ;
      
  (*Printf.printf "yy: %s\n" (IO.to_string (Array.print Float.print) (Bigarray.Array1.to_array yy)) ;
  Printf.printf "yp: %s\n" (IO.to_string (Array.print Float.print) (Bigarray.Array1.to_array yp)) ;
  Printf.printf "residual: %s\n" (IO.to_string (Array.print Float.print) (Bigarray.Array1.to_array res));*)
  0
