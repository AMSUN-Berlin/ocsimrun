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

open Unknowns
open Equations
open Batteries
open Events

open Bigarray
open FlatLayout
open FlatEvents

type fvector = (float, float64_elt, c_layout) Array1.t

let fvector = Array1.create float64 c_layout 

type 'r flatDAE = {
  yy_vec : fvector ;
  yp_vec : fvector ;

  layout : layout ;
  flat_equations : flat_equation array;
  flat_event_state : 'r flat_event_state;
}

let flatten_model us eqs evs = 
  let {dimension;of_unknown;of_equation;equalities} as layout = layout us eqs in
  
  let yy_vec = fvector dimension in
  let yp_vec = fvector dimension in
  
  (* get index to equation handle map *)
  let inv = IntMap.of_enum (Enum.map (fun (key,eq) -> (eq, key)) (IntMap.enum of_equation)) in
  
  let flat_equations = Array.init (dimension - 1 - Array.length equalities) (fun i -> flatten_equation layout (IntMap.find (IntMap.find i inv) eqs)) in
  
  let flat_event_state = init layout evs in

  Printf.printf "Created flat DAE\n%!" ;
  {yy_vec ; yp_vec ; layout ; flat_equations ; flat_event_state  }
