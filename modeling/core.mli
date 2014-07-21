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
open E2lang

type unknown = {
  u_idx : int; 
  u_der : int;
}

type equation_handle

type event_handle

type clock_handle

type relation_handle

module UnknownSet : BatSet.S with type elt = unknown

module UnknownMap : BatMap.S with type key = unknown

module EqSet : BatSet.S with type elt = equation_handle 

module EqMap : BatMap.S with type key = equation_handle 

module EvSet : BatSet.S with type elt = event_handle 

module EvMap : BatMap.S with type key = event_handle 

module RelSet : BatSet.S with type elt = relation_handle 

module RelMap : BatMap.S with type key = relation_handle 

module ClockSet : BatSet.S with type elt = clock_handle 

module ClockMap : BatMap.S with type key = clock_handle 

type 'r core_state_t
constraint 'r = < get_core : 'r core_state_t ; set_core : 'r core_state_t -> 'r ; ..>

type 'r state_trait = <get_core : 'r core_state_t; set_core : 'r core_state_t -> 'r; .. > as 'r
constraint 'r = 'r state_trait

type ('r, 'a) core_monad = 'r state_trait -> ('r state_trait * 'a)

val new_unknown : ('r, unknown) core_monad

val all_unknowns : ('r, unknown BatEnum.t) core_monad

val der : unknown -> ('r, unknown) core_monad

val der_order : unknown -> ('r, int option) core_monad

val unknown_index : unknown -> ('r, int) core_monad

val unknown_mark : ('r, int) core_monad

type 'a attribute = unknown * 'a

val start_values : ('r, float attribute BatEnum.t) core_monad 

val start : float attribute -> ('r, unit) core_monad

type equation = Equality of unknown * unknown  (** equality constraint, i.e. x = y *)
	      | Linear of (unknown array) * (float array) * float  (** linear equation with constant coeffs *)
	      | General of (unknown array) * (stmt array)  (** general, non-linear equation *)

val current_dimension : ('r, int) core_monad

val add_equation : equation -> ('r, equation_handle) core_monad

val all_equations : ('r, equation BatEnum.t) core_monad

val get_equation : equation_handle -> ('r, equation option) core_monad

val del_equation : equation_handle -> ('r, unit) core_monad

val equation_index : equation_handle -> ('r, int) core_monad

val equation_mark : ('r, int) core_monad

val constant : float -> equation

val depends : equation -> UnknownSet.t

type clock = LinearClock of float * float

val add_clock : clock -> ('r, clock_handle) core_monad

val del_clock : clock_handle -> ('r, unit) core_monad

val get_clock : clock_handle -> ('r, clock option) core_monad

val clock_mark : ('r, int) core_monad

val clock_index : clock_handle -> ('r, int) core_monad

type relation_sign = Lt 
		   | Gt

type relation_record = {
  base_rel : equation;
  sign : relation_sign;
}

val add_relation : relation_record -> ('r, relation_handle) core_monad

val relation_index : relation_handle -> ('r, int) core_monad

val get_relation : relation_handle -> ('r, relation_record option) core_monad

val relation_map : ('r, relation_record RelMap.t) core_monad

val relation_handles : ('r, relation_handle BatEnum.t) core_monad

val relation_mark : ('r, int) core_monad

type signal = Or of signal * signal 
	    | And of signal * signal
	    | Relation of relation_handle
	    | Clock of clock_handle
	    | EveryStep

type 'r event = {
  signal : signal ;
  effects : ('r, unit) core_monad ;
}
constraint 'r = < get_core : 'r core_state_t ; set_core : 'r core_state_t -> 'r ; ..>

val add_event : 'r event -> ('r, event_handle) core_monad

val get_event : event_handle -> ('r, 'r event option) core_monad

val all_events : ('r, 'r event BatEnum.t) core_monad

val event_handles : ('r, event_handle BatEnum.t) core_monad

val event_map : ('r, 'r event EvMap.t) core_monad

val del_event : event_handle -> ('r, unit) core_monad

val event_mark : ('r, int) core_monad

class core_state : object('r)
  constraint 'r = < get_core : 'r core_state_t ; set_core : 'r core_state_t -> 'r ; ..>
  val _core_state : 'r core_state_t 
  method get_core : 'r core_state_t
  method set_core : 'r core_state_t -> 'r
end
