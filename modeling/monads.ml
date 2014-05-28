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

(** Monad implmentation in OCaml, originally published in 
    http://blogs.perl.org/users/cyocum/2012/11/writing-state-monads-in-ocaml.html *)

module type MONAD =
  sig
    type 'a t
    val bind : 'a t -> ('a -> 'b t) -> 'b t
    val return : 'a -> 'a t
  end

module type STATE =
  sig
     type t
     val empty : unit -> t
  end

module type STATE_MONAD = 
   functor(State : STATE) ->
     sig
       include MONAD
       val access : 'a t -> 'a
       val put : State.t -> unit t
       val get : State.t t
     end

module MakeStateMonad : STATE_MONAD =
   functor(State : STATE) ->
     struct
       type state = State.t
       type 'a t = state -> ('a * state)
       let bind m f =
         fun s ->
           match m s with 
              | (x, s') -> f x s'
       let return a = fun s -> (a, s)
       let access m =
           match m (State.empty () ) with
             | (x, s) -> x
       let put s =
           fun _ -> ((), s)
       let get =
           fun s -> (s, s)
     end

(** ObjectStateMonad for composable State Monads *)
module ObjectStateMonad =
  struct
    (* A state monad yields tuple of a state-object and an observable value *)
    type ('a, 'b) monad = 'a -> ('a * 'b)
				       
    (* usual bind, just more type parameters *)
    let bind : (('a, 'b) monad) -> ('b -> ('a, 'c) monad) -> ('a, 'c) monad = 
      fun m -> 
      fun f ->
      fun s ->
        let (st, obs) = m(s) in
        ( (f obs) st)

    (* run, as usual *)
    let run m a = m(a)

    type ('a, 'b) field = { field_get : 'a -> 'b ; field_set : 'a -> 'b -> 'a }

    (* get does not directly expose the state but requires a "getter" *)
    let get f = 
      let m : 'a -> ('a * 'b) = fun s -> (s, f.field_get s)
      in m

    (* put requires a "setter" function to modify the state *)
    let put f = 
      fun b ->
      let m : 'a -> ('a * unit) = fun s -> 
	let s2 : 'a = (f.field_set s b) in (s2, ()) 
      in m
	   
    let yield a = fun s -> (s, a)

    let return = yield

    let rec repeat m = function
      | 0 -> m
      | n -> bind m (fun _ -> repeat m (n - 1))		  

  end


