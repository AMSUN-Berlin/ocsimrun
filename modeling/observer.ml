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
open Lens

type observer_state = {
  observed_unknowns : string UnknownMap.t;
}

let empty_obs_state () = {
  observed_unknowns = UnknownMap.empty ;
}

type ('r, 'a) observer_monad = (<get_observer_state : observer_state; set_observer_state : observer_state -> 'r; .. > as 'r) -> ('r * 'a)
constraint 'r = < get_core : 'r core_state_t ; set_core : 'r core_state_t -> 'r ; ..>

let lens = { get = (fun a -> (a#get_observer_state : observer_state)) ; set = fun a b -> b#set_observer_state a }

open Monads.ObjectStateMonad

let observe_as u name = perform (
			    s <-- get lens ;
			    _ <-- put lens {s with observed_unknowns = UnknownMap.add u name s.observed_unknowns} ;
			    return ()
			  )

class state_container = object(self : 'r)
  val _observer_state = empty_obs_state ()
  method get_obs = _observer_state
  method set_obs s = {< _observer_state = s >}
end

