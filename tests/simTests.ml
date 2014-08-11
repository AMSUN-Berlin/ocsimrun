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
let setup _ = new flat_state

let setup_counter _ = (ref 0, new flat_state)

let setup_sample _ = (ref 0., new flat_state)

let teardown _ =
  ()

let test_every_step_sampling (r, s) = ignore (
					  ( perform ( 
						_ <-- add_event { signal=EveryStep ; effects = fun s -> (s, (r := !r + 1)) } ;
						_ <-- SundialsImpl.simulate 
							{ rtol = 0. ; atol = 10e-6 ; minstep = 1. ; start = 0. ; stop = 10. } ;
						return (assert_equal ~msg:"event invocations" ~printer:string_of_int 11 !r)	
					  )) s
				   )

let linear_sample_effect r = perform (
				 osim <-- SundialsImpl.simulation_state ;
				 return (
				     (match osim with 
					Some (sim) -> assert_equal ~msg:"event time" ~printer:string_of_float !r (SundialsImpl.compute_unknown sim time)
				      | None -> assert_bool "no simulation state instance" false 
				     ) ; (r := !r +. 1.) )
			)

let test_linear_sampling (r, s) = ignore (
				      ( perform ( 
					    clock <-- add_clock (LinearClock(1., 0.)) ;
					    _ <-- add_event { signal=Clock(clock) ; effects = linear_sample_effect r } ;
					    _ <-- SundialsImpl.simulate 
						    { rtol = 0. ; atol = 10e-6 ; minstep = 1. ; start = 0. ; stop = 10. } ;
					    return (assert_equal ~msg:"event invocations" ~printer:string_of_float 11. !r)	
				      )) s
				   )

let relation_test r = perform (
			  t <-- sim_value_of time ;
			  return (r := !r + 1 ; assert_equal ~msg:"relation detection" ~printer:string_of_float 1. t) 
			)

let test_relation_detection (r,s) = ignore (
					( perform ( 
					      rel <-- add_relation { base_rel = (Linear ( [| time |], [|1.|],  -1. ) ) ; sign = Gt } ;
					      _ <-- add_event { signal=Relation(rel) ; effects = relation_test r } ;
					      _ <-- SundialsImpl.simulate 
						      { rtol = 0. ; atol = 10e-6 ; minstep = 1. ; start = 0. ; stop = 10. } ;
					      return (assert_equal ~msg:"event invocations" ~printer:string_of_int 1 !r)	
					)) s
				      )

let set_discrete_value r u f = perform (
				   _ <-- sim_set_value u f ;
				   return (r := !r + 1) 
			       )


let assert_discrete_value r u f = perform (
				      f' <-- sim_value_of u ;
				      return (r := !r + 1 ; assert_equal ~msg:"discrete value" ~printer:string_of_float f f') 
				    )

let test_discrete_value (r,s) = ignore (
				    ( perform ( 
					  x <-- new_unknown ;
					  dx <-- der x ;
				      
					  _ <-- add_equation (Linear( [| dx |] , [| 1. |], 0.)) ;
					  
					  rel <-- add_relation { base_rel = (Linear ( [| time |], [|1.|],  -1. ) ) ; sign = Gt } ;
					  _ <-- add_event { signal=Relation(rel) ; effects = assert_discrete_value r x 0. } ;

					  rel <-- add_relation { base_rel = (Linear ( [| time |], [|1.|],  -2. ) ) ; sign = Gt } ;
					  _ <-- add_event { signal=Relation(rel) ; effects = set_discrete_value r x 42. } ;

					  rel <-- add_relation { base_rel = (Linear ( [| time |], [|1.|],  -3. ) ) ; sign = Gt } ;
					  _ <-- add_event { signal=Relation(rel) ; effects = assert_discrete_value r x 42. } ;

					  _ <-- SundialsImpl.simulate 
						  { rtol = 0. ; atol = 10e-6 ; minstep = 1. ; start = 0. ; stop = 10. } ;

					  return (assert_equal ~msg:"event invocations" ~printer:string_of_int 3 !r)	
				    )) s
			      )

let test_start_value s = ignore ( ( perform (
			       x <-- new_unknown ;
			       dx <-- der x ;

			       _ <-- add_equation (Linear( [| dx |] , [| 1. |], 0.)) ;

			       _ <-- start (x, 10.) ;
			       _ <-- SundialsImpl.simulate 
				       { rtol = 0. ; atol = 10e-6 ; minstep = 1. ; start = 0. ; stop = 10. } ;

			       x' <-- sim_value_of x;
			       return (assert_equal ~msg:"simulated_value" ~printer:string_of_float 10. x')
			 ) ) s )

let suite = "Test Core" >:::
  ["test_every_step_sampling" >:: (bracket setup_counter test_every_step_sampling teardown) ;
   "test_relation_detection" >:: (bracket setup_counter test_relation_detection teardown) ;
   "test_discrete_value" >:: (bracket setup_counter test_discrete_value teardown) ;
   "test_linear_sampling" >:: (bracket setup_sample test_linear_sampling teardown) ;
   "test_start_value" >:: (bracket setup test_start_value teardown) ;
  ]

let _ =
  run_test_tt_main suite
