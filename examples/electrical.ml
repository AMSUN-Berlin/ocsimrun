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

open Observer
open Potentials
open Flows

let wire p n s = 
  ( perform ( 
	_ <-- connect_potential p#u n#u ;
	_ <-- connect_flow p#i n#i ;
	return ()
  ) ) s

let one_port name s = 
  ( perform (
	u <-- new_unknown ; 
	_ <-- (observe_as u (name^".u")) ; 
	i <-- new_unknown ; 
	_ <-- (observe_as i (name^".i")) ;
	
	return (object method u = u method i = i end)
  )) s

let two_port name s = 
  ( perform (
	p <-- one_port (name^".p") ;
	n <-- one_port (name^".n") ;

	u <-- new_unknown ; observe_as (u) (name^".u");
	i <-- new_unknown ; observe_as (i) (name^".i");	
	
	(* u = p.u - n.u *)
	_ <-- add_equation (Linear ( [| u ; p#u ; n#u |], [|-1.; 1. ; -1.|], 0. )) ;
	(* inflow = -outflow *)
	_ <-- add_equation (Linear ( [| p#i ; n#i |], [|1.; 1.|], 0. )) ;
	_ <-- add_equation (Equality ( i , p#i ));	
	return (object method u = u method i = i method p = p method n = n end)
  )) s

let ground name s = 
  ( perform (
	p <-- one_port (name^".p") ;
	_ <-- add_equation (Linear([| p#u |] , [| 1. |], 0. ));
	return (object method p = p end) 
  )) s
    
let capacitor name _C s = 
  ( perform (
	super <-- two_port name ;
	du <-- der super#u ;
	observe_as du (name^".u");

	(* C du - i = 0 *)
	_ <-- add_equation (Linear ([| du ; super#i |], [|_C; -1.|], 0.) ); 
	
	return (object method u = super#u method i = super#i method p = super#p method n = super#n end )
  )) s   

let inductor name _I s = 
  ( perform (
	super <-- two_port name ;
	di <-- der super#i ;
	observe_as di (name^".i");

	(* u - I di = 0 *)
	_ <-- add_equation (Linear ([| super#u; di |], [|1.; -1. *. _I|], 0.) ); 
	
	return (object method u = super#u method i = super#i method p = super#p method n = super#n end)
  )) s   

let resistor name _R s = 
  ( perform (
	super <-- two_port name ;
	
	(* Ohm's law: u -Ri = 0 *)
	_ <-- add_equation (Linear ([| super#u; super#i |], [|1.; -1. *. _R|], 0.) ); 
	
	return (object method u = super#u method i = super#i method p = super#p method n = super#n end)
  )) s   

class electrical_state = object (self : 'a)
  inherit Core.core_state 
  inherit Observer.state_container
end
