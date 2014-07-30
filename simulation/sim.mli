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
 *)

open Core

type experiment = {
  rtol : float ;
  atol : float ;
  start : float ;
  minstep : float ;
  stop : float;
}

module type SimEngine = sig
    type simulation_state

    val compute_unknown : simulation_state -> unknown -> float

    type 'r state_trait = <get_sim_state : simulation_state option; set_sim_state : simulation_state option -> 'r; .. > as 'r
    constraint 'r = 'r FlatEvents.state_trait

    type ('r, 'a) sim_monad = 'r -> ('r * 'a)
    constraint 'r = 'r state_trait

    (* TODO: introduce result type *)
    val init : experiment -> ('r, int) sim_monad

    (* one-step simulation (including event handling) *)
    val perform_step : float -> ('r, bool * float) sim_monad

    (* simulation loop w/o initialization *)
    val sim_loop : experiment -> ('r, float) sim_monad

    (* initialization + simulation loop *)
    val simulate : experiment -> ('r, float) sim_monad

    val simulation_state : ('r, simulation_state option) sim_monad

  end

module SundialsImpl : SimEngine

