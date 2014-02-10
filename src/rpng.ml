(*---------------------------------------------------------------------------
   Copyright (c) 2012 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

let err_convert_size = "cannot convert `C8 components to 16 bit pixel formats"
let err_out_of_bounds max len = 
  Printf.sprintf "max index (%d) exceeds array length (%d)" max len
    
exception Error of string
type measure = Unknown | Meter
type compression = No | Best_speed | Best_compression | Default
type pc = L | LA | RGB | RGBA | ARGB
let has_a = function (L | RGB) -> false | _ -> true
let has_rgb = function (L | LA) -> false | _ -> true
  
type ('a, 'b) data = ('a, 'b, Bigarray.c_layout) Bigarray.Array1.t
type ('a, 'b) pf = pc * int * int * int

(* numbers : bytes per comp, array el. per pixel, array el. byte size *)
let l_c8 = L, 1, 1, 1 
let l_c16 = L, 2, 1, 2
let la_c8 = LA, 1, 2, 1
let la_c16 = LA, 2, 2, 2 
let rgb_c8 = RGB, 1, 3, 1
let rgb_c16 = RGB, 2, 3, 2
let rgba_c8 = RGBA, 1, 4, 1
let rgba_c16 = RGBA, 2, 4, 2
let rgba_p8888 = RGBA, 1, 1, 4
let argb_p8888 = ARGB, 1, 1, 4

let data_length ?(w_skip = 0) ~w ~h = function 
| _, _, epp, _ -> (epp * w + w_skip) * h
      
let check_bounds first w_skip w h pf d =
  let max = first + (data_length ~w_skip ~w ~h pf) - 1 in 
  let len = Bigarray.Array1.dim d in 
  if max >= len then invalid_arg (err_out_of_bounds max len) 
    
type comps = [ 
  | `L of [`C8 | `C16 ] 
  | `LA of [`C8 | `C16 ] 
  | `RGB of [`C8 | `C16 ] 
  | `RGBA of [`C8 | `C16 ] ]  

let c_of_depth = function (1|2|4|8) -> `C8 | 16 -> `C16 | _ -> assert false
  
let pc_of_comps pf = 
  let bytes = function `C8 -> 1 | `C16 -> 2 in
  match pf with
  | `L c -> L, (bytes c)
  | `LA c -> LA, (bytes c)
  | `RGB c -> RGB, (bytes c)
  | `RGBA c -> RGBA, (bytes c)
      
let comps_of_pc c = function
| L -> `L c
| LA -> `LA c
| RGB -> `RGB c
| RGBA -> `RGBA c
| _ -> assert false
  
type c
type info = {
  comps : comps;
  width : int; 
  height : int; 
  resolution : (int * int * measure) option; 
  gamma : float option; 
  in_c : in_channel;
  c : c; }

external _codec : reader:bool -> c = "mlpng_codec"
external _input_info : c -> in_channel -> 
  int * int * pc * int * (int * int * measure) option * float option = 
  "mlpng_input_info"

external _set_input_trans : c -> 
  screen_gamma:float option -> src_pc:pc -> src_pcbytes:int -> 
  dst_pc:pc -> dst_pcbytes:int -> packed:bool -> unit =
  "mlpng_bc_set_input_trans" "mlpng_set_input_trans"
    
external _input : c -> offset:int -> row_bytes:int -> h:int -> 
  ('a, 'b) data -> unit = "mlpng_input"
  
external _write : c -> out_channel -> compression  -> 
  res:(int * int * measure) option -> gamma:float option -> pc -> 
  pcbytes:int ->  packed:bool ->  offset:int -> row_bytes:int -> 
  w:int -> h:int -> ('a, 'b) data -> unit = 
  "mlpng_bc_write" "mlpng_write"
    
let input_info ic = 
  try    
    let c = _codec ~reader:true in
    let w, h, pc, depth, res, gamma = _input_info c ic in 
    { comps = comps_of_pc (c_of_depth depth) pc;
      width = w;
      height = h;
      resolution = res;
      gamma = gamma;
      in_c = ic;
      c = c; } 
  with 
  | Invalid_argument e -> raise (Error e)
                            
let input i ?screen_gamma ?(first = 0) ?(w_skip = 0) pf d =
  check_bounds first w_skip i.width i.height pf d;
  let src_pc, src_pcbytes = pc_of_comps i.comps in
  let dst_pc, dst_pcbytes, dst_epp, dst_ebytes = pf in
  let row_bytes = (i.width * dst_epp + w_skip) * dst_ebytes in
  let offset = first * dst_ebytes in
  let p = pf = rgba_p8888 || pf = argb_p8888 in 
  try
    if src_pcbytes < dst_pcbytes then invalid_arg err_convert_size;
    _set_input_trans i.c screen_gamma src_pc src_pcbytes dst_pc dst_pcbytes p;
    _input i.c offset row_bytes i.height d
  with
  | Invalid_argument e -> raise (Error e)
                            
let output oc ?(compress = Default) ?gamma ?res ?(first = 0)  ?(w_skip = 0) 
    ~w ~h pf d =
  check_bounds first w_skip w h pf d;
  let pc, pcbytes, epp, ebytes = pf in
  let row_bytes = (w * epp + w_skip) * ebytes in
  let offset = first * ebytes in
  let packed = pf = rgba_p8888 || pf = argb_p8888 in
  let c = _codec ~reader:false in
  try
    _write c oc compress res gamma pc pcbytes packed offset row_bytes w h d
  with
  | Invalid_argument e -> raise (Error e)

(*---------------------------------------------------------------------------
   Copyright (c) 2012 Daniel C. Bünzli
   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions
   are met:
     
   1. Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

   2. Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

   3. Neither the name of Daniel C. Bünzli nor the names of
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  ---------------------------------------------------------------------------*)
