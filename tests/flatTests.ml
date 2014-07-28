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

class flat_state = object (self : 'a)
  inherit Core.core_state
  inherit Containers.flat_container
end

(* prepare a test state *)
let setup _ = new flat_state 

let teardown _ =
  ()

let test_state_flattening s = ignore (
				  ( perform ( 
					x <-- new_unknown ;
					dx <-- der x;
					layout <-- FlatLayout.flatten ;
					let _ =					      
					  assert_equal ~msg:"flat dimension" ~printer:string_of_int 2 layout.dimension ;
					  assert_equal ~msg:"flatten state" ~printer:string_of_flat_unknown (State(1, 1)) (layout.flatten_unk x)
					in 
					return layout
				  )) s
				)

let suite = "Test Core" >:::
  ["test_state_flattening" >:: (bracket setup test_state_flattening teardown)
  ]

let _ =
  run_test_tt_main suite

