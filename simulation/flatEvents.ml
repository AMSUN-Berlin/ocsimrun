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
  on_clocks : event_handle ClockMap.t;
  on_relations : event_handle RelMap.t;
  on_step : EvSet.t;
  on_unknowns : event_handle UnknownMap.t;
}

type ('r, 'a) event_state_monad = (<get_flat_event_state : 'r flat_event_state; set_flat_event_state : 'r flat_event_state -> 'r; .. > as 'r) -> ('r * 'a)
constraint 'r = < get_core : 'r core_state_t ; set_core : 'r core_state_t -> 'r ; ..>

and 'r flat_event_state = {
  relations : relation_record array;    

  dependencies : dependencies;
  memory : BitSet.t;

  effects : ('r, unit) event_state_monad list;
}
constraint 'r = < get_core : 'r core_state_t ; set_core : 'r core_state_t -> 'r ; 
	   get_flat_event_state : 'r flat_event_state; set_flat_event_state : 'r flat_event_state -> 'r; ..>

type event_roots = float -> fvector -> fvector -> fvector -> int

open Monads.ObjectStateMonad

let event_loop s = (s, () )

let root_found i s = (s, () )

let roots t yy yp gi = 0

let event_roots s = return ( roots ) s

let flat_layout s = flatten s

(*
type sample_point = {
  time : float ; sample : int
}

type flat_signal = FOr of flat_signal * flat_signal 
		 | FAnd of flat_signal * flat_signal
		 | FRel of int
		 | FSmp of int
		 | FEvr

type 'r flat_event = { flat_signal : flat_signal ; flat_effects : 'r effect_gen }

let sample_compare s1 s2 = Float.compare s1.time s2.time

module SampleQueue = Heap.Make(struct type t = sample_point let compare = sample_compare end)


(* use with care, when Pervasives.compare is false-positive, we don't care *)
module SampleMap = Map.Make(struct type t = sample_record let compare = Pervasives.compare end)
module RelMap = Map.Make(struct type t = flat_relation_record let compare = Pervasives.compare end)			       

let vlen = Vect.length
and vempty = Vect.empty
and vadd = Vect.append 
and vget = Vect.get
and vmod = Vect.modify
and vmap = Vect.map
and array_of_vector = Vect.to_array

(* compile an event signal to a form where all samples and relations are indexed - collect on the fly:
     samples (vector), relations (vector), dependencies on "every step" signal (vector), dependencies on samples (vector SampleMap.t),
     dependencies on relations (vector EqMap.t)
 *)
let rec compile_signal layout s r stds sads rds n = function 
    EveryStep -> (s, r, vadd n stds, sads, rds, FEvr)
  | Sample (sr) when SampleMap.mem sr sads -> 
     (s, r, stds, SampleMap.modify sr (vadd n) sads, rds, FSmp(vlen s))
  | Sample (sr) -> (vadd sr s, r, stds, SampleMap.add sr (vadd n vempty) sads, rds, FSmp(vlen s))

  | Relation {relation;sign} -> let feq = {flat_relation=flatten_equation layout relation; flat_sign = sign} in
				if RelMap.mem feq rds then
				  (s, r, stds, sads, RelMap.modify feq (vadd n) rds, FRel(vlen r))
				else 
				  (s, vadd feq r, stds, sads, RelMap.add feq (vadd n vempty) rds, FRel(vlen r))
		       
  | Or(lhs, rhs) -> let (s', r', stds', sads', rds', lhs') = compile_signal layout s r stds sads rds n lhs in
		    let (s'', r'', stds'', sads'', rds'', rhs') = compile_signal layout s r stds sads rds n rhs in
		    (s'', r'', stds'', sads'', rds'', FOr(lhs', rhs'))

  | And(lhs, rhs) -> let (s', r', stds', sads', rds', lhs') = compile_signal layout s r stds sads rds n lhs in
		     let (s'', r'', stds'', sads'', rds'', rhs') = compile_signal layout s r stds sads rds n rhs in
		     (s'', r'', stds'', sads'', rds'', FAnd(lhs', rhs'))

let compile_event layout s r stds sads rds n {signal; effects} = let (s'', r'', stds'', sads'', rds'', flat_signal) = 
								   compile_signal layout s r stds sads rds n signal in
								 (s'', r'', stds'', sads'', rds'', 
								  {flat_signal; 
								   flat_effects = effects})

      
let rec compile layout ces samples relations step_deps sample_deps rel_deps = function
    e :: rest -> let (s', r', stds', sads', rds', c_e) = 
		   compile_event layout samples relations step_deps sample_deps rel_deps (vlen ces) e
		 in
		 compile layout (vadd c_e ces) s' r' stds' sads' rds' rest

  | [] -> (ces, samples, relations, step_deps, sample_deps, rel_deps)


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
		       		       		       
let collect_dependencies deps i rel = 

  let rec add_u (algs, ders) = function
      LowState j when vlen algs > j -> (vmod algs j (vadd i), ders)
    | LowState(j) as u -> add_u (vadd vempty algs, ders) u
    | Algebraic(j) as u when vlen algs > j -> (vmod algs j (vadd i), ders)
    | Algebraic(j) as u -> add_u (vadd vempty algs, ders) u
    | State(j, _) as u when vlen algs > j -> (vmod algs j (vadd i), ders)
    | State(j, _) as u -> add_u (vadd vempty algs, ders) u
    | Derivative j when vlen ders > j -> (algs, vmod ders j (vadd i))
    | Derivative(j) as u -> add_u (algs, vadd vempty ders) u
  in

  Array.fold_left add_u deps (depends rel.flat_relation)

let init layout es = let (ces, s, r, stds, sads, rds) = compile layout vempty vempty vempty vempty SampleMap.empty RelMap.empty es in
		     let samples = array_of_vector s in
		     Printf.printf "Got %d samples\n" (vlen s);
		     let relations  = array_of_vector r in

		     let memory = BitSet.create (vlen r) in
		     let (algs, ders) = Array.fold_lefti collect_dependencies (vempty, vempty) relations in

		     let dependencies = { 
		       on_states = array_of_vector (vmap array_of_vector algs) ; 
		       on_derivatives = array_of_vector (vmap array_of_vector ders) ;
		       on_samples = Array.map (fun s -> array_of_vector (SampleMap.find s sads)) samples;
		       on_relations = Array.map (fun r -> array_of_vector (RelMap.find r rds)) relations;
		       on_step = array_of_vector stds 
		     } in

		     let enqueue q i sr = SampleQueue.add {time = sr.next_t ; sample = i} q in

		     let queue = Array.fold_lefti enqueue SampleQueue.empty samples in
		
		     { flat_events = array_of_vector ces; dependencies; 
		       samples; queue;
		       relations; memory ;
		     }


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
