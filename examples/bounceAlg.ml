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
open Async.Std
open Core.Std

open Unknowns
open Unknowns.Monadic
open Equations
open Equations.Monadic
open Events
open Events.Monadic
open Recon

open Monads.ObjectStateMonad

open Models

let bounce_ball out s = ( 
  perform (
      h <-- new_unknown ;
      v_next <-- new_unknown ;

      _ <-- add_equation ( Linear ( [| der h ; v_next |], [| 0.7 ; 1. |], 0. ) ) ;
      _ <-- add_equation ( Linear ( [| der(der(h)) |], [| 1. |], 9.81 ) ) ;

      let h_start = {
	signal = start_signal ;
	effects = fun _ -> [Unknown (h, 10.) ] ;
      } in

      let h_observe = {
	signal = EveryStep ;
	effects = fun f -> let _ = write_entry out [ f time ; f v_next ; f h ; f (der h) ; f (der (der h)) ] in [Nothing]
      } in
      
      _ <-- add_event h_start ;
      _ <-- add_event h_observe ;

      let bounce = {
	signal = continuous (Linear([| h |], [| 1. |], 0.)) (-1) ;
	effects = fun values -> Printf.printf "Setting v from %f to %f\n" (values (der h)) (values (v_next)) ; 
				[Unknown ((der h), (values (v_next)))]
      } in
      
      _ <-- add_event bounce ;

      return (object method h = h end)
)) s

class bounce_state = object (self : 'a)
  inherit Unknowns.Monadic.state_container 
  inherit Equations.Monadic.state_container
  inherit ['a] Events.Monadic.state_container
end

open Sim

let () = 
  let outfile = File.open_out "results.wall" in
  
  ignore (write_header outfile ["time"; "vnext"; "h"; "h/dt"; "h/dt^2" ]) ;
  
  let mode = instantiate (bounce_ball outfile) (new bounce_state) in
    
  let sim = SundialsImpl.simulate mode { rtol = 0. ; atol = 10e-6 ; start = 0. ; stop = 10. } in 
  let _ = sim >>| (fun res -> (IO.close_out outfile) ; res) >>| function Success -> Printf.printf "shutdown\n" ; Shutdown.shutdown 0 
								  | Error _ -> Printf.printf "error\n" ; Shutdown.shutdown 1 in
  never_returns (Scheduler.go ())
