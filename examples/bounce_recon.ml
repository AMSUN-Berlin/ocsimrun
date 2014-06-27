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

(** bouncing ball with recon output *)
let () = 
  Printf.printf "Bouncing ball example\n%!" (* ;
  let outfile = File.open_out "results.wall" in
  
  ignore (write_header outfile ["time"; "h"; "h/dt"; "h/dt^2" ]) ;
  
  let start_time = Unix.gettimeofday() in

  (* add the observation event *)
  let root s = ( 
    perform (
	ball <-- bounce_ball ;

	let observe = {
	  signal = EveryStep ;
	  effects = fun f -> let _ = write_entry outfile [ f time ; f ball#h ; f (der ball#h) ; f (der (der ball#h)) ] 
			     in [Nothing]
	} in
	
	_ <-- add_event observe ;	

	return ball
  )) s in

  Controlled.run_sim root (new bounce_state) { rtol = 0. ; atol = 10e-6 ; start = 0. ; stop = 10. } *)
 
