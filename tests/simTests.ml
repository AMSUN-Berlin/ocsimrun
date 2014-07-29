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


open Monads.ObjectStateMonad
open OUnit
open Core
open FlatLayout
open Sim

class flat_state = object (self : 'a)
  inherit Core.core_state
  inherit Containers.flat_container
  inherit Containers.sundials_container
end

(* prepare a test state *)
let setup _ = (ref 0, new flat_state)

let teardown _ =
  ()

let test_every_step_sampling (r, s) = ignore (
					  ( perform ( 
						_ <-- add_event { signal=EveryStep ; effects = fun s -> (s, (r := !r + 1)) } ;
						_ <-- SundialsImpl.simulate 
							{ rtol = 0. ; atol = 10e-6 ; minstep = 1. ; start = 0. ; stop = 10. } ;
						return (assert_equal ~msg:"event invocations" ~printer:string_of_int 10 !r)	
					  )) s
				   )

let test_linear_sampling (r, s) = ignore (
				      ( perform ( 
					    clock <-- add_clock (LinearClock(1., 0.)) ;
					    _ <-- add_event { signal=Clock(clock) ; effects = fun s -> (s, (r := !r + 1)) } ;
					    _ <-- SundialsImpl.simulate 
						    { rtol = 0. ; atol = 10e-6 ; minstep = 1. ; start = 0. ; stop = 10. } ;
					    return (assert_equal ~msg:"event invocations" ~printer:string_of_int 10 !r)	
				      )) s
				   )



let suite = "Test Core" >:::
  ["test_every_step_sampling" >:: (bracket setup test_every_step_sampling teardown) ;
   "test_linear_sampling" >:: (bracket setup test_linear_sampling teardown) 
  ]

let _ =
  run_test_tt_main suite
