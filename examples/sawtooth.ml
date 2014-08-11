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
open Monads.ObjectStateMonad
open Recon

let sawtooth out s = ( 
  perform (
      x <-- new_unknown ;
      dx <-- der x ;
      _ <-- add_equation ( Linear ( [| dx |], [| 1. |], 1. ) ) ;

      _ <-- start (x, 1.) ;


      let x_observe = {
	signal = EveryStep ;
	requires_reinit = false;
	effects = perform ( 
		      vals <-- sim_values_of [ time; x ] ;
		      return (ignore (write_entry out vals) );
		    ) 
      } in
      
      _ <-- add_event x_observe ;

      top <-- add_relation { base_rel = Linear([| x |], [| 1. |], 0.); sign = Lt } ;

      let reinit = {
	signal = Relation top ;
	requires_reinit = true;
	effects = sim_set_value x 1. ;
      } in
      
      _ <-- add_event reinit ;

      return (object method x = x end)
)) s

open Sim

class sawtooth_state = object (self : 'a)
  inherit Core.core_state
end

let () = 
  let outfile = File.open_out "results.wall" in
  
  write_header outfile ["time"; "x" ] ;

  ()
  (*
  let mode = instantiate (sawtooth outfile) (new sawtooth_state) in

  let sim = SundialsImpl.simulate mode { rtol = 0. ; atol = 10e-6 ; start = 0. ; stop = 10. } in 

  ignore (Lwt_main.run sim)
   *)
