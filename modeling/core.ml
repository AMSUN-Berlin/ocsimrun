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

type 'a attribute = unknown * 'a

type equation_handle = int

type event_handle = int

type clock_handle = int

type relation_handle = int

let string_of_unknown {u_idx;u_der} = Printf.sprintf "d^%d(x_%d)/dt" u_der u_idx 

let unknown_compare u1 u2 = BatOrd.comp0 (BatOrd.bin_ord Int.ord u1.u_idx u2.u_idx Int.ord u1.u_der u2.u_der)

module UnknownSet = struct 
  include Set.Make(struct type t = unknown let compare = unknown_compare end)
		  
  let of_list us = List.fold_right add us empty
end

module UnknownMap = Map.Make(struct type t = unknown let compare = unknown_compare end)

type unknown_state_t = {
  unknown_count : int ;
  unknown_set : UnknownSet.t ;
  start_values : float UnknownMap.t ;
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

type sim_interface_t = {
  value_of : unknown -> float ;
  set_value : unknown -> float -> unit;
}

module SimState = struct type t = sim_interface_t 
			 let empty () = { value_of = (fun _ -> raise (Failure "No simulation setup"))  ; 
					  set_value = (fun _ _ -> raise (Failure "No simulation setup")) } 
		  end
module Sim = struct 
  include Monads.MakeStateMonad(SimState)
  let value_of u = perform (
		       s <-- get ;
		       return (s.value_of u) ;
		     )

  let values_of us = perform (
			 s <-- get ;
			 return (List.map (fun u -> s.value_of u) us) ;
		       )


  let set_value u f = perform (
			  s <-- get ;
			  return (s.set_value u f) ;
			)
end


type 'r event = {
  signal : signal ;
  requires_reinit : bool;
  effects : ('r, unit) core_monad;
}
constraint 'r = < get_core : 'r core_state_t ; set_core : 'r core_state_t -> 'r ; ..>

and 'r core_state_t = {
  unknowns : unknown_state_t;

  events : 'r event handle_store;

  equations : equation handle_store;
  
  clocks : clock handle_store;

  relations : relation_record handle_store;

  sim_interface : sim_interface_t;
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

let dtime = {u_idx = 0; u_der = 1}

open Lens

module UnknownState = struct type t = unknown_state_t let empty () = {unknown_set = UnknownSet.add dtime (UnknownSet.singleton time) ; unknown_count = 1 ; start_values = UnknownMap.empty} end
module Unknowns = struct 
  include Monads.MakeStateMonad(UnknownState)

  let unknowns = { get = (fun a -> a.unknowns) ; set = fun b a -> {a with unknowns = b} }

  let cardinality = perform (
			{unknown_set} <-- get ;
			return (UnknownSet.cardinal unknown_set)
		      )


  let new_unknown = perform (
			{unknown_count ; unknown_set ; start_values } <-- get ;
			let u = {u_idx = unknown_count; u_der = 0} in
			_ <-- put { unknown_count = unknown_count + 1; unknown_set = UnknownSet.add u unknown_set ; start_values } ;
			return u 
		      )

  let all_unknowns = perform (
			 {unknown_set} <-- get ;
			 return (UnknownSet.enum unknown_set)
		       )
		      
  let del_unknown u = perform (
			  {unknown_count ; unknown_set ; start_values } <-- get ;
			  _ <-- put { unknown_count = unknown_count + 1; unknown_set = UnknownSet.add u unknown_set ; start_values = UnknownMap.remove u start_values } ;
			  return ()
			)

  let unknown_index u = perform (
    {unknown_set} <-- get ;
    match UnknownSet.split_opt u unknown_set with
    | (l, Some _, _) -> return (UnknownSet.cardinal l)
    | (_, None, _) -> raise (Failure (Printf.sprintf "The unknown <%d,%d> is not element of the current model." u.u_idx u.u_der))
  )

  let der {u_idx;u_der} = perform
      {unknown_count ; unknown_set ; start_values } <-- get ;
      let du = {u_idx;u_der=u_der+1} in
      _ <-- put { unknown_count ; unknown_set = UnknownSet.add du unknown_set ; start_values } ;
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

  let start_values s = ( perform (
			     state <-- get ;
			     return (UnknownMap.enum state.start_values) ;
		       ) ) s

  let set_start_attr (u, f) = perform (
				  state <-- get ;
				  _ <-- put {state with start_values = UnknownMap.add u f state.start_values} ;
				  return () ;
				)
				 
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

  let all s = ( perform (
		    {count;store} <-- get ;
		    return (IntMap.values store)
	      ) ) s

  let handles s = (perform (
		     {count;store} <-- get ;
		     return (IntMap.keys store)
		) ) s

  let store s = (perform ( 
		     {count;store} <-- get ;
		     return store
		) ) s


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

  (* a simple mock for the simulation interface *)
  sim_interface = {
    value_of = (fun _ -> Float.nan) ;
    set_value = (fun _ _ -> ()) ;
  }
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

let simulation = { get = (fun a -> a.sim_interface) ; set = fun a b -> {b with sim_interface = a} }

open Monads.ObjectStateMonad
open Lens.Infix 

let sim_value_of u = using (core |-- simulation) (Sim.value_of u)

let sim_values_of us = using (core |-- simulation) (Sim.values_of us)

let set_sim_interface i = using (core |-- simulation) (Sim.put i)

let sim_set_value u f = using (core |-- simulation) (Sim.set_value u f)

let new_unknown s = (using (core |-- unknowns) Unknowns.new_unknown) s

let all_unknowns s = (using (core |-- unknowns) Unknowns.all_unknowns) s

let unknown_index u = using (core |-- unknowns) (Unknowns.unknown_index u)

let unknown_mark s = using (core |-- unknowns) Unknowns.mark s

let start_values s = using (core |-- unknowns) Unknowns.start_values s

let start attr = using (core |-- unknowns) (Unknowns.set_start_attr attr)

let der_order u = using (core |-- unknowns) (Unknowns.der_order u)

let der u = using (core |-- unknowns) (Unknowns.der u)

let current_dimension s = let (s', n) = (using ( core |-- equations) Basic.cardinality s) in (s', n+1)

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

let relation_handles s = using ( core |-- relations) Basic.handles s

let get_relation h = using ( core |-- relations ) (Basic.get_el h)

let relation_map s = using ( core |-- relations) Basic.store s

let add_clock e = using ( core |-- clocks ) (Basic.add e)

let del_clock h = using ( core |-- clocks ) (Basic.del h)

let clock_index h = using ( core |-- clocks ) (Basic.index_of h)

let get_clock h = using ( core |-- clocks ) (Basic.get_el h)

let clock_mark s = using (core |-- clocks) Basic.mark s

let all_clocks s = using ( core |-- clocks) Basic.all s

let clock_handles s = using ( core |-- clocks) Basic.handles s

let add_event e = using (core |-- events) (Basic.add e)

let del_event h = using (core |-- events) (Basic.del h)

let get_event h = using (core |-- events) (Basic.get_el h)

let event_index h = using (core |-- events) (Basic.index_of h)

let event_mark s = using (core |-- events) Basic.mark s

let all_events s = using ( core |-- events) Basic.all s

let event_handles s = using ( core |-- events) Basic.handles s

let event_map s = using ( core |-- events) Basic.store s
