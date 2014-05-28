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
open Events
open Events.Monadic
open Recon

open Monads.ObjectStateMonad

open Models

let rec up x s = ( 
  perform (
      e <-- add_equation ( Linear ( [| der(x) |], [| 1. |], -1.0 ) ) ;
      return (object method e = e end)
)) s

and down x s = (
  perform (
      e <-- add_equation ( Linear ( [| der(x) |], [| 1. |], 1.0 ) ) ;      
      return (object method e = e end)
)) s

let parent_mode s = (
  perform (
      x <-- new_unknown ;
      
      mode <-- up x ;

      (*
      let switch_to_down r = (
	perform (	    
	    del_equation mode#e ;
	    mode2 <-- down x ;
	    return ({del_unknowns = ISet.empty ; del_eqns = ISet.of_list [mode#e]; del_events = ISet.empty}, 
		    {add_unknowns = UnknownSet.empty ; add_equations = [mode2#e]; add_events = []}
		   )
      )) r in

      let switch = {
	signal = continuous (Linear([| x |], [| 1. |], -1.)) (1) ;
	effects = fun _ -> [Model (switch_to_down)]
      } in

       *)
      return (object method x = x end) 
)) s

class state = object (self : 'a)
  inherit Unknowns.Monadic.state_container 
  inherit Equations.Monadic.state_container
  inherit ['a] Events.Monadic.state_container
end

open Sim

let () =  
  let model = instantiate (parent_mode) (new state) in
    
  (match SundialsImpl.simulate model { rtol = 0. ; atol = 10e-6 ; start = 0. ; stop = 10. } with
     Success -> Printf.printf "# Success!\n"
   | Error (n, msg) -> Printf.printf "# Error %d: %s\n" n msg );
  
