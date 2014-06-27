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

open Lwt
 
let server_port = 12345
let so_timeout = Some 20
let backlog = 10
 
let try_close chan =
  catch (fun () -> Lwt_io.close chan)
	(function _ -> return ())
 
let init_socket sockaddr =
  let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Lwt_unix.setsockopt socket Unix.SO_REUSEADDR true;
  Lwt_unix.bind socket sockaddr;
  Lwt_unix.listen socket backlog;
  socket
 
let process socket ~timeout ~callback =
  let rec _process () =
    Lwt_unix.accept socket >>=
      (fun (socket_cli, _) ->
       let inchan = Lwt_io.of_fd ~mode:Lwt_io.input socket_cli in
       let outchan = Lwt_io.of_fd ~mode:Lwt_io.output socket_cli in
       let c = callback inchan outchan in
       let events =
	 match timeout with
	 | None -> [c]
	 | Some t -> [c; Lwt_unix.sleep (float_of_int t) >> return ()]
       in
       ignore (Lwt.pick events >> try_close outchan >> try_close inchan);
       _process ()
      )
  in
  _process ()
 

open Core
open Commands_t
open Commands_j
(*

let setup = 
  let sockaddr = Unix.ADDR_INET (Unix.inet_addr_any, server_port) in
  let socket = init_socket sockaddr in

  let d = Lwt_mvar.create_empty () in
  let c = Lwt_mvar.create_empty () in

  let rec control_sample _ = Lwt.map (function Some n -> Printf.printf "next control sync at %f\n" n ; Some { Events.next_t = n ; schedule = control_sample}
					     | _ -> None
				     ) (Lwt_mvar.take c) in

  { }

let run_sim model empty_state options =

  (* add the observation event *)
  let root s = ( 
    perform (
	out <-- model ;

	obs <-- get Observer.field ;

	let state = Observer.observer_array obs in

	let observe = {
	  signal = Sample { Events.next_t = 0.1 ; Events.schedule = control_sample };
	  effects = fun f -> let _ = Observer.update_observer_array f state obs ; Lwt_mvar.put d (`SimState state) in [Nothing];
	} in
	
	_ <-- add_event observe ;	

	return out
  )) s in

  let mode = Models.instantiate root empty_state in
  
  let sim_proc = Sim.SundialsImpl.simulate mode options in

  let handle_command = function
      `Continue n -> Lwt_mvar.put c n >> Lwt_mvar.take d >|= string_of_output
    | `Finish -> Lwt_mvar.put c None >> Lwt.return "finishing"
    | _ -> Lwt.return "Command not implemented"
  in

  let callback inchan outchan = 
    Lwt_io.read_line inchan >>= (fun msg -> (Printf.printf "read: '%s'\n" msg) ; 					    
					    handle_command (command_of_string msg) >>= (fun out -> Lwt_io.write_line outchan out))
  in

  Lwt_main.run (
      process
	socket
	~timeout:so_timeout
	~callback:callback
    )
 *)
