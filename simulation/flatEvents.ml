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
open FlatLayout

(** book-keeping of dependencies between events and possible sources *)
type dependencies = {
  on_clocks : EvSet.t ClockMap.t;
  on_relations : EvSet.t RelMap.t;
  on_step : EvSet.t;
  on_unknowns : EvSet.t UnknownMap.t;
}

type event_marks = {
  clm : int;
  rlm : int;
  evm : int;
}

type flat_relation_record = {
  feq : flat_equation ;
  sign : relation_sign ;
}

type sample_point = {
  time : float ; clock : clock_handle
}

let sample_compare s1 s2 = Float.compare s1.time s2.time

module SampleQueue = Heap.Make(struct type t = sample_point let compare = sample_compare end)

type 'r state_trait = <get_event_state : 'r flat_event_state option; set_event_state : 'r flat_event_state option -> 'r; ..> as 'r
constraint 'r = 'r Core.state_trait

and ('r, 'a) event_state_monad = 'r -> ('r * 'a)
constraint 'r = 'r state_trait

and 'r flat_event_state = {
  layout : layout ;
  relations : flat_relation_record array;    
  relation_handles : relation_handle array;
  relation_indices : int RelMap.t;

  dependencies : dependencies;
  memory : BitSet.t;

  effects : ('r, unit) event_state_monad list;
  roots : fvector -> fvector -> fvector -> int;

  queue : SampleQueue.t ;

  marks : event_marks ;
}
constraint 'r = 'r state_trait

type event_roots = fvector -> fvector -> fvector -> int

open Monads.ObjectStateMonad

open Lens

let state = { get = (fun a -> a#get_event_state) ; set = fun a b -> b#set_event_state a }

let eadd = EvSet.add
let eempty = EvSet.empty

let layout es = es.layout


(* parse a signal and collect on the fly:
     dependencies on "every step", dependencies on clocks ,
     dependencies on relations
 *)
let collect_deps (stds, cds, rds) (eh,ev) =
  
  let rec sig_collect_deps (stds,cds,rds) = function
      EveryStep -> (eadd eh stds, cds, rds)

    | Clock (ch) when ClockMap.mem ch cds -> 
       (stds, ClockMap.modify ch (eadd eh) cds, rds)

    | Clock (ch) -> (stds, ClockMap.add ch (eadd eh eempty) cds, rds)

    | Relation (rh) when RelMap.mem rh rds ->
       (stds, cds, RelMap.modify rh (eadd eh) rds)

    | Relation (rh) ->
       (stds, cds, RelMap.add rh (eadd eh eempty) rds)
		       
    | Or(lhs, rhs) -> let (stds', cds', rds') = sig_collect_deps (stds,cds,rds) lhs in
		      sig_collect_deps (stds',cds',rds') rhs

    | And(lhs, rhs) -> let (stds', cds', rds') = sig_collect_deps (stds,cds,rds) lhs in
		       sig_collect_deps (stds',cds',rds') rhs

  in sig_collect_deps (stds, cds, rds) ev.signal

let effects_exist s = ( perform (
			   so <-- get state ;
			   return (match so with Some s -> s.effects = [] | None -> false)
		     ) ) s

let is_valid s = perform (
		     c <-- clock_mark ;
		     e <-- event_mark ;
		     r <-- relation_mark ;
		     return (s.marks.clm = c && s.marks.rlm = r && s.marks.evm = e) 			    
		 )

let flatten_relation layout rh (hds, index_map, frs) = perform (
							   o <-- get_relation rh ;
							   return
							     (match o with
								Some rel -> 
								let feq = layout.flatten_eq rel.base_rel in
								(* append the handle, remember the handle's index and append flat-relation *)
								(Vect.append rh hds, RelMap.add rh (Vect.length hds) index_map, 
								 Vect.append { feq ; sign = rel.Core.sign } frs)
							      | None -> (hds, index_map, frs) ) ;
							 )

let event_marks s = ( perform ( 
			  evm <-- event_mark ;
			  clm <-- clock_mark ;
			  rlm <-- relation_mark ;
			  return { clm ; rlm ; evm }
		    )) s

let flat_layout s = ( perform (
			  so <-- get state ;
			  layout <-- (match so with
					Some (es) -> FlatLayout.validate es.layout 
				      | None -> FlatLayout.flatten );
			  return layout
		    )) s

let flatten s = ( perform (
		      layout <-- flat_layout ;
		      evs <-- event_map ;
		      let (stds, cds, rds) = Enum.fold collect_deps (eempty, ClockMap.empty, RelMap.empty) (EvMap.enum evs) in (* collect dependencies from events *)

		      rhs <-- relation_handles ;

		      (* calculate the flat-relation array, storing the handle-to-index map and the index-to-handle array *)
		      (handles,index_map,frs) <-- fold_enum (flatten_relation layout) (Vect.empty, RelMap.empty, Vect.empty) rhs;

		      let memory = BitSet.create (Vect.length frs) in

		      let dependencies = { 
			on_unknowns = UnknownMap.empty ; (* TODO: fill *)
			on_clocks = cds; on_step = stds; on_relations = rds;
		      } in

		      marks <-- event_marks ;

		      let relations = Vect.to_array frs in

		      let roots yy yp gi = 
			let calc_root i r = 
			  gi.{i} <- layout.compute_eq yy yp relations.(i).feq
			in
			(Array.iteri calc_root relations) ; 0 in

		      let es = { layout ; relations; relation_indices = index_map ; relation_handles = Vect.to_array handles ; 
				 dependencies ; roots ; memory ; queue = SampleQueue.empty ; effects = [] ; marks } in

		      _ <-- put state (Some(es));
		      return es
		)) s	      

let validate es = perform (
		      v <-- is_valid es ;
		      if (v) then return es else flatten  
		    )

let event_state s = ( perform (
			  so <-- get state ;
			  match so with 
			    Some es -> validate es 
			  | None -> flatten
		    )) s

type signal_src = SigClock of clock_handle | SigRel of int * bool

let rec eval src index rel_state = function 
    (* ever is some kind of special sample *)
    EveryStep -> (match src with SigClock(_) -> (true, true) | _ -> (true, false) )
	      
  | Clock(s) -> ( match src with SigClock(r) -> (s = r, s = r) | _ -> (false, false) )
		 
  | Relation(s) -> ( let idx = index s in
		     match src with SigRel(r, b) when idx = r -> (b, (BitSet.mem rel_state idx) <> b)
				  | _ -> (BitSet.mem rel_state idx, false) )

  | Or(s1, s2) -> let (a1, a2) = eval src index rel_state s1 in
		  let (b1, b2) = eval src index rel_state s2 in
		  (* minimal form computed by wolfram alpha, using <> to substitute for 'xor' *)
		  (a1 || b1, (a1 && b2) <> (a2 && b1) <> (a2 && b2) <> a2 <> b2)

  | And(s1, s2) -> let (a1, a2) = eval src index rel_state s1 in
		   let (b1, b2) = eval src index rel_state s2 in
		   (* minimal form computed by wolfram alpha, using <> to substitute for 'xor' *)
		   (a1 || b1, (a1 && b2) <> (a2 && b1) <> (a2 && b2))

let rec event_loop yy yp = perform (
			   es <-- event_state ;
			   match es.effects with
			     [] -> return () ;
			   | effects -> (seq effects) &. event_loop yy yp			   
			 )

let collect_effects mem idx src eh effects = perform (
						 o <-- get_event eh ;
						 match o with Some ev ->
							      let (v, c) = eval src idx mem ev.signal in
							      if (v && c) then return (ev.Core.effects :: effects)
							      else return effects 
							    | _ -> return effects
					       )

let root_found i sign = perform (
			    es <-- event_state ;
			    let rel_index rh = RelMap.find rh es.relation_indices in
			    let rh = es.relation_handles.(i) in
			    let rel = es.relations.(i) in
			    let v = sign = rel.sign in
			    let src = SigRel(i, v) in
			    let deps = (EvSet.enum (RelMap.find rh es.dependencies.on_relations)) in
			    effects <-- fold_enum (collect_effects es.memory rel_index src) es.effects deps ;
			    let _ = BitSet.put es.memory v i in
			    _ <-- put state (Some { es with effects = effects });
			    return ()
			  )

let event_array_size s = ( perform (
			       es <-- event_state ;
			       return (Array.length es.relations) 
			 ) ) s

let event_roots s = ( perform ( 
			  es <-- event_state ;
			  return ( es.roots ) 
		    ) ) s

  
let do_reset yy yp es i r = let v = (Float.signbit (es.layout.compute_eq yy yp r.feq)) && (r.sign = Lt) in
			    BitSet.put es.memory v i

let reset_roots yy yp = perform (
			    es <-- event_state ;
			    return (Array.iteri (do_reset yy yp es) es.relations)
			  )

let next_clock s = ( perform (
			 es <-- event_state ;
			 return (
			   if SampleQueue.size es.queue = 0 then None
			   else (Some (SampleQueue.find_min es.queue).time) )
		   ) ) s
  
let sample yy yp ch = function 
    LinearClock(a, b) -> { time = yy.{0} *. a +. b ; clock = ch }

let schedule yy yp ch = perform (
			    es <-- event_state ;
			    o <-- get_clock ch ;
			    match o with Some(clock) ->					      
					 let sp = sample yy yp ch clock in
                               		 let queue' = SampleQueue.insert es.queue sp in
					 put state (Some {es with queue = queue'})
				       | None -> return ()
			  )

let rec advance_queue t queue clocks =
  if (SampleQueue.size queue = 0) then (queue, clocks) else
    let s = (SampleQueue.find_min queue) in
    if s.time <= t then (
      advance_queue t (SampleQueue.del_min queue) (s.clock :: clocks)
    ) else 
      (queue, clocks)

let fire_clock yy yp ch = perform (
			      es <-- event_state ;
			      let deps = (EvSet.enum (ClockMap.find ch es.dependencies.on_clocks)) in
			      let src = SigClock(ch) in
			      let rel_index rh = RelMap.find rh es.relation_indices in

			      effects' <-- fold_enum (collect_effects es.memory rel_index src) es.effects deps ;			      
			      _ <-- schedule yy yp ch ;
			      put state (Some {es with effects = effects'})
			    )

let rec fire_clocks yy yp = function 
    [] -> return ()
  | ch :: clocks -> perform (
			_ <-- fire_clock yy yp ch ;
			fire_clocks yy yp clocks ;
		      )
			    
let reschedule yy yp = perform (
			   es <-- event_state ;
			   let (queue', active_clocks) = advance_queue yy.{0} es.queue [] in
			   _ <-- put state (Some {es with queue = queue'}) ;
			   fire_clocks yy yp active_clocks ;
			 )
