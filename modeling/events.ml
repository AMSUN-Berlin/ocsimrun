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

open Lwt

open Equations
open Unknowns
open Batteries

type sample_record = { 
  next_t : float;
  schedule : (unknown -> float) -> sample_record option Lwt.t ;
}

type relation_record = {
  relation : equation;
  sign : int; 
}

type signal = Or of signal * signal 
	    | And of signal * signal
	    | Relation of relation_record
	    | Sample of sample_record
	    | EveryStep

let continuous relation sign = Relation { relation; sign }

let start_signal = Sample {
  next_t = Float.neg_infinity ;
  schedule = fun _ -> Lwt.return None
}

type 'r effect = Equation of int * equation
	      | Unknown of unknown * float
	      | Model of ('r -> 'r * unit) (* model change wraps an object monad of same type *)
	      | Nothing (* TODO: remove this and just use empty list ? *)

and 'r effect_gen = (unknown -> float) -> ('r effect) list

and 'r event = {
  signal : signal ;
  effects : 'r effect_gen ;
}

module IMap = Map.Make(Int)

module type MANAGER = sig 
    type 'r state

    val init : ('r event) list -> 'r state

    (* val update : state -> ISet.t -> event IMap.t -> state *)

    val relations : 'r state -> relation_record array

    val next_step : 'r state -> float option

    val update_mem : 'r state -> (equation -> float) -> unit

    val reschedule : 'r state -> float -> ('r effect_gen) list -> ('r effect_gen) list * 'r state
					 
    val relation_fired : 'r state -> int -> int -> (equation -> float) -> ('r effect_gen) list -> ('r effect_gen) list

    val relations_for_unknown : 'r state -> unknown -> int list
  end

module Monadic = struct
  open Batteries
  open Monads.ObjectStateMonad
  module IntMap = Map.Make(Int)

  type 'r state_t = int * 'r event IntMap.t

  class ['r] state_container = object
    val _events : 'r state_t = (0, IntMap.empty)
    method get_events = _events
    method set_events s = {< _events = s >} 
  end

  let field = { field_get = (fun a -> (a#get_events : 'r state_t)) ; field_set = fun a b -> a#set_events b }

  let add_event e s = ( perform (
			    (n, es) <-- get field ;
			    put field (n+1, IntMap.add n e es) ;
			    return n
		       )) s

end
