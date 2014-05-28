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
open Msgpack
open IO

let wall_header = "recon:wall:v01"

let enc s = `FixRaw (List.of_enum (String.enum s))

let write_header o signals = 
  let header = `FixMap [
		  (enc"fmeta", `FixMap[]);
		  (enc"tabs", `FixMap[(enc"signals", `FixMap [
							(enc "tmeta", `FixMap []);
							(enc "sigs", `FixArray (List.map enc signals));
							(enc "als", `FixMap []);
							(enc "vmeta", `FixMap [])
				     ])]
	       )] in
  let bytes = Msgpack.Serialize.serialize_string header in
  let length = String.length bytes in
  
  nwrite o wall_header ;
  BigEndian.write_i32 o length ;
  nwrite o bytes ; 

  length

let encf v = `Double v 

let write_entry o values =
  let entry = `FixMap [ 
		 (enc "signals", `FixArray (List.map encf values)) ] in
  let bytes = Msgpack.Serialize.serialize_string entry in
  let length =  String.length bytes in
  BigEndian.write_i32 o length ;
  nwrite o bytes ; 
  
  length
