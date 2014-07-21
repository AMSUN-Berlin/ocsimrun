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

open Core
open FlatLayout

type dependencies

type 'r state_trait = <get_event_state : 'r flat_event_state option; set_event_state : 'r flat_event_state option -> 'r; ..> as 'r
constraint 'r = 'r Core.state_trait

and 'r flat_event_state
constraint 'r = 'r state_trait

and ('r, 'a) event_state_monad = 'r -> ('r * 'a)
constraint 'r = 'r state_trait

val is_valid : 'r flat_event_state -> ('r, bool) event_state_monad

val flatten : ('r, 'r flat_event_state) event_state_monad

val event_state : ('r, 'r flat_event_state) event_state_monad

val validate : 'r flat_event_state -> ('r, 'r flat_event_state) event_state_monad

val flat_layout : ('r, FlatLayout.layout) event_state_monad

type event_roots = fvector -> fvector -> fvector -> int

val event_roots : ('r, event_roots) event_state_monad

val root_found : int -> relation_sign -> ('r, unit) event_state_monad 

val reset_roots : fvector -> fvector -> ('r, unit) event_state_monad 

val event_loop : fvector -> fvector -> ('r, unit) event_state_monad

val next_clock : ('r, float option) event_state_monad 

val schedule : fvector -> fvector -> clock_handle -> ('r, unit) event_state_monad

val reschedule : fvector -> fvector -> ('r, unit) event_state_monad 

