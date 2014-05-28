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

(** an unknown is a entry of two dimensions: derivation and number*)
type unknown = {
  u_idx : int; 
  u_der : int;
}

(** derivation simply counts up the derivation index *)
let der u = {u with u_der = u.u_der + 1}

let compare u1 u2 = BatOrd.comp0 (BatOrd.bin_ord Int.ord u1.u_idx u2.u_idx Int.ord u1.u_der u2.u_der)

module UnknownSet = struct 
  include Set.Make(struct type t = unknown let compare = compare end)
		  
  let of_list us = List.fold_right add us empty
end

let time = {u_idx = 0; u_der = 0}

module UnknownMap = Map.Make(struct type t = unknown let compare = compare end)

module Monadic = struct
  open Batteries
  open Monads.ObjectStateMonad

  type handle_t = int

  type state_t = int * UnknownSet.t

  class state_container = object
    val _unknowns : state_t = (1, UnknownSet.singleton time)
    method get_unknowns = _unknowns
    method set_unknowns (n,us) = {< _unknowns = (n,us) >} 
  end

  let field = { field_get = (fun a -> (a#get_unknowns : state_t)) ; field_set = fun a b -> a#set_unknowns b }

  let new_unknown s = ( 
    perform (
	(n, us) <-- get field ;
	let u = { u_idx = n; u_der = 0} in
	put field (n+1, UnknownSet.add u us) ;
	return u
  )) s

end
