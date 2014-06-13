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

open Batteries
open Unknowns
open Unknowns.Monadic
open Equations
open Equations.Monadic

open Monads.ObjectStateMonad

open Observer

module IntSet = Set.Make(Int)

type potential_sets = (IntSet.t * UnknownSet.t) UnknownMap.t

class potential_container = object
  val _potentials : potential_sets = UnknownMap.empty ;
  method get_potentials = _potentials 
  method set_potentials ps = {< _potentials = ps >}
end

let potentials = { field_get = (fun a -> (a#get_potentials : potential_sets)) ; field_set = fun a b -> a#set_potentials b }

let merge_potentials (eqs1, eq_set1) (eqs2, eq_set2) = 
	if (UnknownSet.equal eq_set1 eq_set2) then 
	  return (eqs1, eq_set1) 
	else
	  fun s -> ( perform ( 
			 (* assume the sets are distinct *)
			 let u1 = UnknownSet.min_elt eq_set1 in
			 let u2 = UnknownSet.min_elt eq_set2 in
			 eq <-- add_equation (Equality(u1, u2)) ;
			 return (IntSet.add eq (IntSet.union eqs1 eqs2), UnknownSet.union eq_set1 eq_set2) 
		   ) ) s
				   

let add_to_potential (eqs, eq_set) u s = 
  ( perform (	
	eq <-- add_equation (Equality(u, UnknownSet.min_elt eq_set)) ;
	return (IntSet.add eq eqs, UnknownSet.add u eq_set)
  )) s

let make_potential p_u n_u s = 
  ( perform (
	eq <-- add_equation (Equality(p_u, n_u)) ;
	return (IntSet.singleton eq, UnknownSet.add p_u (UnknownSet.singleton n_u))
  )) s

let connect_potential p_u n_u s =
  ( perform ( 
	pots <-- get potentials ;

	(eqs, eq_set) <-- 
	  if UnknownMap.mem p_u pots then (
	    let (eqs, eq_set) = UnknownMap.find p_u pots in
	    if UnknownMap.mem n_u pots then
	      merge_potentials (eqs, eq_set) (UnknownMap.find n_u pots)
	    else
	      add_to_potential (eqs, eq_set) n_u
	  ) else
	      make_potential p_u n_u ;

	let pots' = (UnknownMap.add n_u (eqs, eq_set) (UnknownMap.add n_u (eqs, eq_set) pots)) in
	_ <-- put potentials pots' ;
	return () ;
  )) s
