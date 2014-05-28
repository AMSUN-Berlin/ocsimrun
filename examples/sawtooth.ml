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

open Monads.ObjectStateMonad

open Models
open Recon

let sawtooth out s = ( 
  perform (
      x <-- new_unknown ;
      _ <-- add_equation ( Linear ( [| der(x) |], [| 1. |], 1. ) ) ;

      let x_start = {
	signal = start_signal ;
	effects = fun _ -> [Unknown (x, 1.) ] ;
      } in

      let x_observe = {
	signal = EveryStep ;
	effects = fun f -> let _ = write_entry out [ f time ; f x ; ] in [Nothing]
      } in
      
      _ <-- add_event x_start ;
      _ <-- add_event x_observe ;

      let reinit = {
	signal = continuous (Linear([| x |], [| 1. |], 0.)) (-1) ;
	effects = fun _ -> [Unknown (x, 1.)]
      } in
      
      _ <-- add_event reinit ;

      return (object method x = x end)
)) s

open Sim

class sawtooth_state = object (self : 'a)
  inherit Unknowns.Monadic.state_container 
  inherit Equations.Monadic.state_container
  inherit ['a] Events.Monadic.state_container
end

let () = 
  let outfile = File.open_out "results.wall" in
  
  write_header outfile ["time"; "x" ] ;

  let model = instantiate (sawtooth outfile) (new sawtooth_state) in
  
  match SundialsImpl.simulate model { rtol = 0. ; atol = 10e-6 ; start = 0. ; stop = 10. } with
    Success -> Printf.printf "# Success!\n"
  | Error (n, msg) -> Printf.printf "# Error %d: %s\n" n msg
