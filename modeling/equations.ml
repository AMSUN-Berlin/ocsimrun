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

open E2lang
open Unknowns

(** Equations can be specialized *)
type equation = Equality of unknown * unknown  (** equality constraint, i.e. x = y *)
	      | Linear of (unknown array) * (float array) * float  (** linear equation with constant coeffs *)
	      | General of (unknown array) * (stmt array)  (** general, non-linear equation *)

let depends = function Equality(u1, u2)  -> UnknownSet.add u1 (UnknownSet.singleton u2)
		     | Linear(us, _, _) -> Array.fold_right UnknownSet.add us UnknownSet.empty
		     | General(us, _) -> Array.fold_right UnknownSet.add us UnknownSet.empty

module Monadic = struct
  open Batteries
  open Monads.ObjectStateMonad  
  module IntMap = Map.Make(Int)

  type state_t = int * equation IntMap.t

  class state_container = object
    val _del_equations : ISet.t = ISet.empty
    val _equations : state_t = (0, IntMap.empty)

    method get_equations = _equations
    method set_equations s = {< _equations = s >} 
    method mark_equations = {< _del_equations = ISet.empty >}
  end

  let field = { field_get = (fun a -> (a#get_equations : state_t)) ; field_set = fun a b -> a#set_equations b }

  let add_equation e s = ( perform (
			       (n, es) <-- get field ;
			       put field (n+1, IntMap.add n e es) ;
			       return n
			 )) s

end
