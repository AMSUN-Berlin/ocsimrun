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

type clock = LinearClock of float * float

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

type sample_point = {
  time : float ; clock : clock_handle
}

let sample_compare s1 s2 = Float.compare s1.time s2.time

module SampleQueue = Heap.Make(struct type t = sample_point let compare = sample_compare end)

type 'r event = {
  signal : signal ;
  effects : ('r, unit) core_monad ;
}
constraint 'r = < get_core : 'r core_state_t ; set_core : 'r core_state_t -> 'r ; ..>

and 'r core_state_t = {
  unknowns : unknown_state_t;

  events : 'r event handle_store;

  equations : equation handle_store;
  
  clocks : clock handle_store;

  relations : relation_record handle_store;

  clock_queue : SampleQueue.t ;
}
constraint 'r = < get_core : 'r core_state_t ; set_core : 'r core_state_t -> 'r ; ..>

and 'r state_trait = <get_core : 'r core_state_t; set_core : 'r core_state_t -> 'r; .. > as 'r
constraint 'r = 'r state_trait

and ('r, 'a) core_monad = 'r state_trait -> ('r state_trait * 'a)
constraint 'r = 'r state_trait

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

let empty_handle_store () = { count = 0; store = IntMap.empty }

(* all core relations follow the same implementation pattern *)
module Basic = struct 
  let bind m f =
    fun s ->
    match m s with 
    | (s', x) -> f x s'

  let return a = fun s -> (s, a)
  
  let access m =
    match m (empty_handle_store () ) with
    | (s, x) -> x
  let put s =
    fun _ -> (s, ())
  let get =
    fun s -> (s, s)
	       
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

  let all = perform (
		{count;store} <-- get ;
		return (IntMap.values store)
	      )

  let mark s = ( perform (
		     s <-- get ;
		     return s.count ;
	       ) ) s

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

  clock_queue = SampleQueue.empty ;

}

class core_state = object(self : 'r)
  val _core_state : 'r core_state_t = empty_core_state ()
  method get_core = _core_state
  method set_core cs = {< _core_state = cs >} 
end
		     
let core : 'r core_lens = { get = (fun a -> (a#get_core : 'r core_state_t)) ; set = fun a b -> (b#set_core a) }

let equations = { get = (fun a -> a.equations) ; set = fun a b -> {b with equations = a} }

let relations = { get = (fun a -> a.relations) ; set = fun a b -> {b with relations = a} }

let clocks = { get = (fun a -> a.clocks) ; set = fun a b -> {b with clocks = a} }

let events = { get = (fun a -> a.events) ; set = fun a b -> {b with events = a} }

module QueueState = struct type t = unknown_state_t let empty () = {unknown_set = UnknownSet.singleton time ; unknown_count = 1} end
module ClockQueue = struct 
  include Monads.MakeStateMonad(QueueState)

  let peek = perform (
		 q <-- get ;
		 return (if (SampleQueue.size q = 0) then None else Some (SampleQueue.find_min q).time)
	       )


  let rec _pop t q cs = if SampleQueue.size q = 0 then (q, cs) 
			else
			  let n = (SampleQueue.find_min q) in
			  if n.time <= t then (
			    let q' = SampleQueue.del_min q in 
			    (q', n.clock :: cs)
			  ) else
			    (q, cs)

  let push t ch = perform (
		      q <-- get ;
		      _ <-- put (SampleQueue.add {time=t; clock=ch} q) ;
		      return () 
		    )

  let pop t = perform (
		      q <-- get ;
		      let (q', cs) = _pop t q [] in
		      _ <-- put q' ;
		      return cs 
		    )
			  
end

let clock_queue = { get = (fun a -> a.clock_queue) ; set = fun b a -> {a with clock_queue = b} }

open Monads.ObjectStateMonad
open Lens.Infix 

let new_unknown s = (using (core |-- unknowns) Unknowns.new_unknown) s

let all_unknowns s = (using (core |-- unknowns) Unknowns.all_unknowns) s

let unknown_index u = using (core |-- unknowns) (Unknowns.unknown_index u)

let unknown_mark s = using (core |-- unknowns) Unknowns.mark s

let der_order u = using (core |-- unknowns) (Unknowns.der_order u)

let der u = using (core |-- unknowns) (Unknowns.der u)

let current_dimension s = using ( core |-- equations ) Basic.cardinality s

let add_equation e = using ( core |-- equations ) (Basic.add e)

let del_equation h = using ( core |-- equations ) (Basic.del h)

let all_equations s = using ( core |-- equations ) Basic.all s

let equation_index h = using ( core |-- equations ) (Basic.index_of h)

let equation_mark s = using (core |-- equations) Basic.mark s

let get_equation h = using ( core |-- equations ) (Basic.get_el h)

let add_relation e = using ( core |-- relations ) (Basic.add e)

let relation_mark s = using (core |-- relations) Basic.mark s

let del_relation h = using ( core |-- relations ) (Basic.del h)

let relation_index h = using ( core |-- relations ) (Basic.index_of h)

let get_relation h = using ( core |-- relations ) (Basic.get_el h)

let add_clock e = using ( core |-- clocks ) (Basic.add e)

let del_clock h = using ( core |-- clocks ) (Basic.del h)

let clock_index h = using ( core |-- clocks ) (Basic.index_of h)

let get_clock h = using ( core |-- clocks ) (Basic.get_el h)

let clock_mark s = using (core |-- clocks) Basic.mark s

let reschedule t ch = perform (
			  c <-- get_clock ch ;
			  let t' = match c with
			      Some LinearClock(a, b) -> t*.a +. b 
			  in
			  _ <-- (using (core |-- clock_queue) (ClockQueue.push t' ch)) ;
			  return t';
			)

let advance_time t = using (core |-- clock_queue) (ClockQueue.pop t)

let peek_next_time s = using (core |-- clock_queue) ClockQueue.peek s

let add_event e = using (core |-- events) (Basic.add e)

let del_event h = using (core |-- events) (Basic.del h)

let get_event h = using (core |-- events) (Basic.get_el h)

let event_index h = using (core |-- events) (Basic.index_of h)

let event_mark s = using (core |-- events) Basic.mark s

