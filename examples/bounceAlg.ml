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
open Recon

open Monads.ObjectStateMonad

let bounce_ball out = 
  perform (
      h <-- new_unknown ;
      v_next <-- new_unknown ;
      
      v <-- der h ;
      a <-- der v ;

      _ <-- start (h, 10.) ;

      _ <-- add_equation ( Linear ( [| v ; v_next |], [| 0.7 ; 1. |], 0. ) ) ;
      _ <-- add_equation ( Linear ( [| a |], [| 1. |], 9.81 ) ) ;

      let h_observe = {
	signal = EveryStep ;
	effects = perform ( 
		      t_ <-- sim_value_of time ;
		      v_ <-- sim_value_of v;
		      vn_ <-- sim_value_of v_next ;
		      a_ <-- sim_value_of a ;
		      h_ <-- sim_value_of h ;
		      return (ignore (write_entry out [ t_ ; vn_ ; h_ ; v_ ; a_ ]))
		    )
      } in
      
      _ <-- add_event h_observe ;

      contact <-- add_relation { base_rel = Linear([| h |], [| 1. |], 0.) ; sign = Lt } ;

      let bounce = {
	signal = Relation contact ;
	effects = perform ( 
		      vn_ <-- sim_value_of v_next ;
		      _ <-- sim_set_value v vn_ ;		      
		      return () 
			  )
      } in
      
      _ <-- add_event bounce ;

      return (object method h = h end)
)

class bounce_state = object (self : 'a)
  inherit Core.core_state
end

open Sim

let () = 
  let outfile = File.open_out "results.wall" in
  
  ignore (write_header outfile ["time"; "vnext"; "h"; "h/dt"; "h/dt^2" ]) ;
  (*
  let mode = instantiate (bounce_ball outfile) (new bounce_state) in
    
  let sim = SundialsImpl.simulate mode { rtol = 0. ; atol = 10e-6 ; start = 0. ; stop = 10. } in 

 ignore (Lwt_main.run sim)*)
