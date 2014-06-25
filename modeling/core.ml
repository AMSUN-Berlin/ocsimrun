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
open E2lang
open Lens

(** an unknown is a entry of two dimensions: derivation and number*)
type unknown = {
  u_idx : int; 
  u_der : int;
}

type equation_handle = int

type event_handle = int

type clock_handle = int

type relation_handle = int

let unknown_compare u1 u2 = BatOrd.comp0 (BatOrd.bin_ord Int.ord u1.u_idx u2.u_idx Int.ord u1.u_der u2.u_der)

module UnknownSet = struct 
  include Set.Make(struct type t = unknown let compare = unknown_compare end)
		  
  let of_list us = List.fold_right add us empty
end

module UnknownMap = Map.Make(struct type t = unknown let compare = unknown_compare end)

type unknown_state_t = {
  unknown_count : int ;
  unknown_set : UnknownSet.t ;
}

module IntSet = Set.Make(Int)
module IntMap = Map.Make(Int)

module EqSet = IntSet

module EqMap = IntMap

module EvSet = IntSet

module EvMap = IntMap

module RelSet = IntSet

module RelMap = IntMap

module ClockSet = IntSet

module ClockMap = IntMap

(** Equations can be specialized *)
type equation = Equality of unknown * unknown  (** equality constraint, i.e. x = y *)
	      | Linear of (unknown array) * (float array) * float  (** linear equation with constant coeffs *)
	      | General of (unknown array) * (stmt array)  (** general, non-linear equation *)

type relation_sign = Lt | Gt

type clock_record = {
  base_val : equation;
  schedule : float -> clock_record option;
}

type relation_record = {
  base_rel : equation;
  sign : relation_sign;
}

type signal = Or of signal * signal 
	    | And of signal * signal
	    | Relation of relation_handle
	    | Clock of clock_handle
	    | EveryStep

type 'a handle_store = { count : int ; store : 'a IntMap.t } 

type 'r event = {
  signal : signal ;
  effects : 'r effect_gen ;
}
constraint 'r = < get_core : 'r core_state_t ; set_core : 'r core_state_t -> 'r ; ..>

and 'r effect = 
  | Equation of int * equation
  | Unknown of unknown * float
  | ReSample of float
  | Model of ('r, unit) core_monad
constraint 'r = < get_core : 'r core_state_t ; set_core : 'r core_state_t -> 'r ; ..>

and 'r effect_gen = (unknown -> float) -> ('r effect) list
constraint 'r = < get_core : 'r core_state_t ; set_core : 'r core_state_t -> 'r ; ..>

and 'r core_state_t = {
  unknowns : unknown_state_t;

  events : 'r event handle_store;

  equations : equation handle_store;
  
  clocks : clock_record handle_store;

  relations : relation_record handle_store;
}
constraint 'r = < get_core : 'r core_state_t ; set_core : 'r core_state_t -> 'r ; ..>

and ('r, 'a) core_monad = (<get_core : 'r core_state_t; set_core : 'r core_state_t -> 'r; .. > as 'r) -> ('r * 'a)

type 'r core_lens = ('r, ('r core_state_t)) Lens.t 
constraint 'r = < get_core : 'r core_state_t ; set_core : 'r core_state_t -> 'r ; ..>

type event_handler = int

let time = {u_idx = 0; u_der = 0}

open Lens

module UnknownState = struct type t = unknown_state_t let empty () = {unknown_set = UnknownSet.singleton time ; unknown_count = 1} end
module Unknowns = struct 
  include Monads.MakeStateMonad(UnknownState)

  let unknowns = { get = (fun a -> a.unknowns) ; set = fun b a -> {a with unknowns = b} }

  let new_unknown = perform (
			{unknown_count ; unknown_set} <-- get ;
			let u = {u_idx = unknown_count; u_der = 0} in
			_ <-- put { unknown_count = unknown_count + 1; unknown_set = UnknownSet.add u unknown_set } ;
			return u 
		      )

  let all_unknowns = perform (
			 {unknown_count ; unknown_set } <-- get ;
			 return (UnknownSet.enum unknown_set)
		       )
		      
  let del_unknown u = perform
      {unknown_count ; unknown_set} <-- get ;
      _ <-- put { unknown_count = unknown_count + 1; unknown_set = UnknownSet.add u unknown_set } ;
      return ()

  let unknown_index u = perform (
    {unknown_count; unknown_set} <-- get ;
    match UnknownSet.split_opt u unknown_set with
    | (l, Some _, _) -> return (UnknownSet.cardinal l)
    | (_, None, _) -> raise (Failure (Printf.sprintf "The unknown <%d,%d> is not element of the current model." u.u_idx u.u_der))
  )

  let der {u_idx;u_der} = perform
      {unknown_count ; unknown_set} <-- get ;
      let du = {u_idx;u_der=u_der+1} in
      _ <-- put { unknown_count ; unknown_set = UnknownSet.add du unknown_set } ;
      return du

  let der_order u = perform
      {unknown_count ; unknown_set} <-- get ;
      let next_base = {u_idx=u.u_idx+1; u_der=0} in
      match (UnknownSet.split_opt next_base unknown_set) with 
      | (l, _, _) when UnknownSet.is_empty l -> return None
      | (l, _, _) when (UnknownSet.max_elt l).u_idx <> u.u_idx -> return None
      | (l, _, _) -> return ( Some (UnknownSet.max_elt l).u_der )

  let mark = perform 
	       { unknown_count ; unknown_set} <-- get ;
	     return unknown_count
end

let unknowns = { get = (fun a -> a.unknowns) ; set = fun a b -> {b with unknowns = a} }

module type CORE_TYPE = sig 
    type el_t
  end

let empty_handle_store () = { count = 0; store = IntMap.empty }

(* all core relations follow the same implementation pattern *)
module CoreRelation = 
  functor(Type : CORE_TYPE) -> 
	 struct
	   module StateMonad = Monads.MakeStateMonad(struct type t = Type.el_t handle_store  let empty = empty_handle_store end)
						    
	   open StateMonad
	   
	   let cardinality = perform (
				 {count;store} <-- get ;
				 return (IntMap.cardinal store)
			       )
	   
	   let add e = perform (
			   {count;store} <-- get ;
			   _ <-- put {count = count + 1; store = IntMap.add count e store} ;
			   return count 
			 )

	   let del e = perform (
			   s <-- get ;
			   _ <-- put {store = IntMap.remove e s.store ; count = s.count + 1} ;
			   return ()
			 )

	   let index_of e = perform (
				{count; store} <-- get ;
				match IntMap.split e store with
				| (l, Some _, _) -> return (IntMap.cardinal l)
				| (_, None, _) -> raise (Failure (Printf.sprintf "The handle %d is not element of the current model." e))
			      )

	   let mark = perform (
			  s <-- get ;
			  return s.count ;
			)

	   let get_el e = perform (
			      s <-- get ;			   
			      return (IntMap.Exceptionless.find e s.store)
			    )

	 end

let constant f = Linear([||], [||], f)

let depends = function Equality(u1, u2)  -> UnknownSet.add u1 (UnknownSet.singleton u2)
		     | Linear(us, _, _) -> Array.fold_right UnknownSet.add us UnknownSet.empty
		     | General(us, _) -> Array.fold_right UnknownSet.add us UnknownSet.empty

let empty_core_state () = {
  unknowns = UnknownState.empty () ;

  events = empty_handle_store () ;

  equations = empty_handle_store () ;

  clocks = empty_handle_store () ;

  relations = empty_handle_store () ;
}

class core_model = object(self : 'r)
  val _core_state : 'r core_state_t = empty_core_state ()
  method get_core = _core_state
  method set_core cs = {< _core_state = cs >} 
end
		     
let core : 'r core_lens = { get = (fun a -> (a#get_core : 'r core_state_t)) ; set = fun a b -> (b#set_core a) }

module Equations = CoreRelation ( struct type el_t = equation end )

let equations = { get = (fun a -> a.equations) ; set = fun a b -> {b with equations = a} }

module Relations = CoreRelation ( struct type el_t = relation_record end )

let relations = { get = (fun a -> a.relations) ; set = fun a b -> {b with relations = a} }

module Clocks = CoreRelation ( struct type el_t = clock_record end )

let clocks = { get = (fun a -> a.clocks) ; set = fun a b -> {b with clocks = a} }

open Monads.ObjectStateMonad
open Lens.Infix 

let new_unknown s = (using (core |-- unknowns) Unknowns.new_unknown) s

let all_unknowns s = (using (core |-- unknowns) Unknowns.all_unknowns) s

let unknown_index u = using (core |-- unknowns) (Unknowns.unknown_index u)

let unknown_mark s = using (core |-- unknowns) Unknowns.mark s

let der_order u = using (core |-- unknowns) (Unknowns.der_order u)

let der u = using (core |-- unknowns) (Unknowns.der u)

let current_dimension s = using ( core |-- equations ) Equations.cardinality s

let add_equation e = using ( core |-- equations ) (Equations.add e)

let del_equation h = using ( core |-- equations ) (Equations.del h)

let equation_index h = using ( core |-- equations ) (Equations.index_of h)

let equation_mark s = using (core |-- equations) Equations.mark s

let get_equation h = using ( core |-- equations ) (Equations.get_el h)

let add_relation e = using ( core |-- relations ) (Relations.add e)

let del_relation h = using ( core |-- relations ) (Relations.del h)

let relation_index h = using ( core |-- relations ) (Relations.index_of h)

let get_relation h = using ( core |-- relations ) (Relations.get_el h)

let add_clock e = using ( core |-- clocks ) (Clocks.add e)

let del_clock h = using ( core |-- clocks ) (Clocks.del h)

let clock_index h = using ( core |-- clocks ) (Clocks.index_of h)

let get_clock h = using ( core |-- clocks ) (Clocks.get_el h)
