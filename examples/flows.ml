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
open Core
open Monads.ObjectStateMonad

open Observer

type flow_sets = equation_handle UnknownMap.t

class flow_container = object 
  val _flows : flow_sets = UnknownMap.empty ;
  method get_flows = _flows 
  method set_flows fs = {< _flows = fs >}
end

let flows = { field_get = (fun a -> (a#get_flows : flow_sets)) ; field_set = fun a b -> a#set_flows b }

let merge_sums s1 s2 s = ( perform (
			       Some eq1 <-- get_equation s1 ;
			       Some eq2 <-- get_equation s2 ;

			       del_equation s1 ;
			       del_equation s2 ;

			       let us = Array.of_enum (UnknownSet.enum (UnknownSet.union (depends eq1) (depends eq2))) in
			       sum <-- add_equation (Linear(us, (Array.map (fun _ -> 1.) us), 0.)) ;
			       return sum;
			 )) s

let add_to_sum sum u s = ( perform (
			       Some eq1 <-- get_equation sum ;
			       _ <-- del_equation sum ;

			       let us = Array.of_enum (UnknownSet.enum (UnknownSet.add u (depends eq1))) in
			       sum <-- add_equation (Linear (us, (Array.map (fun _ -> 1.) us), 0.)) ;
			       return sum;
			 )) s

let connect_flow p_i n_i s = 
  ( perform (
	fls <-- get flows ;
	sum <--
	  if UnknownMap.mem p_i fls then
	    let p_eq_idx = UnknownMap.find p_i fls in
	    if UnknownMap.mem n_i fls then (
	      let n_eq_idx = UnknownMap.find n_i fls in
	      merge_sums p_eq_idx n_eq_idx ;
	    ) else (
	      add_to_sum p_eq_idx n_i ;
	    )
	  else (
	    (* p_i + n_i = 0. *)
	    add_equation (Linear ([| p_i ; n_i |] , [| 1. ; 1. |] , 0.)) ;
	  ) ;
	       
	_ <-- put flows (UnknownMap.add n_i sum (UnknownMap.add p_i sum fls));
	return () ;	    
  )) s
