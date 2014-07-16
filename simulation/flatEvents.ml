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

type 'r state_trait = <get_event_state : 'r flat_event_state option; set_event_state : 'r flat_event_state option -> 'r; ..> as 'r
constraint 'r = 'r Core.state_trait

and ('r, 'a) event_state_monad = 'r -> ('r * 'a)
constraint 'r = 'r state_trait

and 'r flat_event_state = {
  layout : layout ;
  relations : flat_relation_record array;    

  dependencies : dependencies;
  memory : BitSet.t;

  effects : ('r, unit) event_state_monad list;

  marks : event_marks;
}
constraint 'r = 'r state_trait

type event_roots = float -> fvector -> fvector -> fvector -> int

open Monads.ObjectStateMonad

open Lens

let state = { get = (fun a -> a#get_event_state) ; set = fun a b -> b#set_event_state a }

let eadd = EvSet.add
let eempty = EvSet.empty


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

let is_valid s = perform (
		     c <-- clock_mark ;
		     e <-- event_mark ;
		     r <-- relation_mark ;
		     return (s.marks.clm = c && s.marks.rlm = r && s.marks.evm = e) 			    
		 )

let flat_layout s = ( perform (
			  so <-- get state ;
			  layout <-- (match so with
					Some (es) -> FlatLayout.validate es.layout 
				      | None -> FlatLayout.flatten );
			  return layout
		    )) s

let flatten_relation layout rh frs = perform (
					 o <-- get_relation rh ;
					 return
					   (match o with
					      Some rel -> 
					      let feq = layout.flatten_eq rel.base_rel in
					      Vect.append { feq ; sign = rel.Core.sign } frs
					    | None -> frs ) ;
				       )

let event_marks s = ( perform ( 
			  evm <-- event_mark ;
			  clm <-- clock_mark ;
			  rlm <-- relation_mark ;
			  return { clm ; rlm ; evm }
		    )) s

let flatten s = ( perform (
		      layout <-- flat_layout ;
		      evs <-- event_map ;
		      let (stds, cds, rds) = Enum.fold collect_deps (eempty, ClockMap.empty, RelMap.empty) (EvMap.enum evs) in (* collect dependencies from events *)

		      relations <-- relation_handles ;

		      frs <-- fold_enum (flatten_relation layout) Vect.empty relations;

		      let memory = BitSet.create (Vect.length frs) in

		      let dependencies = { 
			on_unknowns = UnknownMap.empty ; (* TODO: fill *)
			on_clocks = cds; on_step = stds; on_relations = rds;
		      } in

		      marks <-- event_marks ;

		      let es = { layout ; relations = Vect.to_array frs; dependencies ; memory ; effects = [] ; marks } in

		      return es
		)) s	      

let validate es = perform (
		      v <-- is_valid es ;
		      if (v) then return es else flatten  
		    )

let get s = ( perform (
		  so <-- get state ;
		  match so with 
		    Some es -> validate es 
		  | None -> flatten
	    )) s

let event_loop yy yp s = (s, () )

let root_found i s = (s, () )

let roots t yy yp gi = 0

let event_roots s = return ( roots ) s

(*




type signal_src = SigSmp of int | SigRel of int * bool

let rec eval src rel_state = function 
    (* ever is some kind of special sample *)
    FEvr -> (match src with SigSmp(_) -> (true, true) | _ -> (true, false) )
	      
  | FSmp(s) -> ( match src with SigSmp(r) -> (s = r, s = r) | _ -> (false, false) )
		 
  | FRel(s) -> let v = BitSet.mem rel_state s in
	       ( match src with SigRel(r, b) when s = r -> (b, v <> b)
			      | _ -> (v, false)
	       )

  | FOr(s1, s2) -> let (a1, a2) = eval src rel_state s1 in
		   let (b1, b2) = eval src rel_state s2 in
		   (* minimal form computed by wolfram alpha, using <> to substitute for 'xor' *)
		   (a1 || b1, (a1 && b2) <> (a2 && b1) <> (a2 && b2) <> a2 <> b2)

  | FAnd(s1, s2) -> let (a1, a2) = eval src rel_state s1 in
		    let (b1, b2) = eval src rel_state s2 in
		    (* minimal form computed by wolfram alpha, using <> to substitute for 'xor' *)
		    (a1 || b1, (a1 && b2) <> (a2 && b1) <> (a2 && b2))
		       		       		       


let collect state src effs e = 
  let (v', c) = eval src state.memory e.flat_signal in
  if v' && c then (* evaluates to true and has changed *)
    e.flat_effects::effs
  else 
    effs

(** walk over all events and check whether the relation change did actually change the value of the event signal to true *)
let relation_fired state i sign f gs = let r = state.relations.(i) in
				       let v = sign = r.flat_sign in
				       Printf.printf "Relation %d %b -> %b\n" i (BitSet.mem state.memory i) v ;
				       let src = SigRel(i, v) in
				       let effects = Array.fold_left (collect state src) gs state.flat_events in
				       Printf.printf "Yields %d effect-generators\n" (List.length effects) ;
				       (* collect effects _before_ storing the change *)
				       BitSet.put state.memory v i ; effects
								    
let next_step state = if (SampleQueue.size state.queue = 0) then None else Some (SampleQueue.find_min state.queue).time
  
let fire_sample state effs i = let src = SigSmp(i) in
				 Array.fold_left (collect state src) effs state.flat_events

let rec do_reschedule effs state samples t =
  if (SampleQueue.size state.queue = 0) then (effs, state, samples) else
    let s = (SampleQueue.find_min state.queue) in
    if s.time <= t then (
      let effs' = fire_sample state effs s.sample in
      do_reschedule effs' {state with queue = SampleQueue.del_min state.queue} (s::samples) t
    ) else 
      (effs, state, samples)
    
let relations_for_unknown state = function
    State(i,_) -> state.dependencies.on_states.(i)
  | Algebraic(i) -> state.dependencies.on_states.(i)
  | LowState(i) -> state.dependencies.on_states.(i)
  | Derivative i -> state.dependencies.on_derivatives.(i)


let reschedule state t gs = if (SampleQueue.size state.queue = 0) then 
			      (* when our queue is _empty_ we need to fire a dummy event to get the EVER effects *)
			      (* TODO: introduce real dummy source *)
			      (fire_sample state gs (-1), state, []) 
			    else do_reschedule gs state [] t

let relations state = state.relations
  
let do_update state f i r = let v = (Float.signbit (f r.flat_relation)) && (r.flat_sign < 0) in
			    BitSet.put state.memory v i

let update_mem state f = Array.iteri (do_update state f) state.relations

open Lwt

let next_state state f samples = 
  let enqueue_sample queue point = (state.samples.(point.sample).schedule f) >>=
				     (* we evaluate the samples on demand and update the corresponding point *)
				     function Some sample' -> state.samples.(point.sample) <- sample' ; queue >|= SampleQueue.add {point with time = sample'.next_t} 
					    | None -> queue in
  
  (List.fold_left enqueue_sample (return state.queue) samples) >|= fun queue -> {state with queue=queue}
 *)
