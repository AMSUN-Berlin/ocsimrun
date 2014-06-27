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

type geometry = Box of float * float * float 
	      | Sphere of float * int
	      | Plane of float * float * int * int

type color = Rgba of int * int * int * int 

type material = {
  color : color;
}

type vis_obj = {
  geometry : geometry;
  material : material option;
}

type change =     
    Move of equation * equation * equation 
  | Scale of equation
  | EulerRotate of equation * equation * equation 

type transformation_time =
    Always 
  | Once

type transformation = {
  change : change;
  at : transformation_time;  
}

module IntMap = Map.Make(Int)

(** allocate buffer for numeric visualisation state *)
let alloc_buffer t_map =
  let (max, _) = IntMap.max_binding t_map in
  let buff = Array.make max [||] in

  let alloc_transform = function Move(_,_,_) | EulerRotate(_,_,_) -> [|0., 0., 0.|] 
				 | Scale(_) -> [||]
  in
  let alloc_transforms obj ts = buff.(obj) <- Array.of_list(List.map alloc_transform ts) in
  IntMap.iter alloc_transforms t_map

(** update the numeric visualisation state *)
let update_buffer state buff t_map = 
  let write_transform b = function Move(x,y,z) -> b.(0) <- state x ; b.(1) <- state y ; b.(2) <- state z
				 | Scale(f) -> b.(0) <- state f 
				 | EulerRotate(x,y,z) -> b.(0) <- state x ; b.(1) <- state y ; b.(2) <- state z 
  in
  let write_transforms obj ts = List.iter2 write_transform buff.(obj) ts in
  
  IntMap.iter write_transforms t_map

module Monadic = struct
  open Batteries
  open Monads.ObjectStateMonad
  open Lens
	 
  type state_t = {
    counter : int;
    objects : vis_obj IntMap.t;
    transformations : transformation list IntMap.t;
  }

  class state_container = object
    val state : state_t = { counter = 0; objects = IntMap.empty ; transformations = IntMap.empty }
    method get_visualisation = state
    method set_visualisation st = {< state = st >}
  end

  let field = { get = (fun a -> (a#get_visualisation : state_t)) ; set = fun a b -> b#set_visualisation a }

  let add_obj obj s = ( perform ( 
			    scene <-- get field ;
			    let scene' = {scene with counter=scene.counter+1; objects = IntMap.add scene.counter obj scene.objects } in
			    put field scene';
			    return (scene.counter + 1);
		      )) s

  let box w h d s = add_obj {geometry=Box(w,h,d); material=None} s

  let sphere r seg s = add_obj {geometry=Sphere(r, seg); material=None} s

  let add_transform obj f s = ( 
    perform ( 
	scene <-- get field ;
        let trans' = IntMap.modify_def [f] obj (fun l -> f::l) scene.transformations in
        let scene' = {scene with transformations=trans'} in
	put field scene' ;
	return ()
  ) ) s


end
