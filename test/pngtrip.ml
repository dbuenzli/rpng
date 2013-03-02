(*---------------------------------------------------------------------------
   Copyright %%COPYRIGHT%%. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% version %%VERSION%%
  ---------------------------------------------------------------------------*)

open Bigarray;;

let pr = Format.fprintf
let str f = Printf.sprintf f

let exec = Filename.basename Sys.executable_name
let log f = Format.eprintf ("%s: " ^^ f ^^ "@?") exec 
let log_error inf e = Format.eprintf "%s: %s: %s\n" exec inf e 

(* IO tools *) 

let apply f x ~finally y = 
  let res = try f x with exn -> finally y; raise exn in
  finally y;
  res

let close_ic ic = try close_in ic with Sys_error e -> log "%s\n" e; exit 1
let ic_for inf =
  try if inf = "-" then stdin else open_in inf with 
  | Sys_error e -> log "%s\n" e; exit 1

(* Info *)

let info inf = 
  let ic = ic_for inf in 
  let comps_to_string c = 
    let bits = function `C8 -> 8 | `C16 -> 16 in
    match c with 
    | `L c -> str "L (%d bits)" (bits c)
    | `LA c -> str "LA (%d bits)" (bits c)
    | `RGB c -> str "RGB (%d bits)" (bits c)
    | `RGBA c -> str "RGBA (%d bits)" (bits c)
  in
  let res_to_string r = 
    let m_to_string = function
    | Rpng.Unknown -> "unknown unit"
    | Rpng.Meter -> "pixels/meters"
    in
    match r with
    | None -> "none"
    | Some (rw, rh, m) -> str "%d x %d (%s)" rw rh (m_to_string m)
  in
  let gamma_to_string = function
  | None -> "none"
  | Some g -> str "%F" g
  in
  try
    let i = Rpng.input_info ic in
    let pr = Format.printf in
    pr "%s:\n" inf;
    pr "  components: %s\n" (comps_to_string i.Rpng.comps);
    pr "  size: %d x %d\n" i.Rpng.width i.Rpng.height;
    pr "  resolution: %s\n" (res_to_string i.Rpng.resolution);
    pr "  gamma : %s%!\n" (gamma_to_string i.Rpng.gamma);
    close_ic ic
  with
  | (Sys_error e | Rpng.Error e) -> log_error inf e; close_ic ic
  | End_of_file -> log_error inf "unexpected end of file"; close_ic ic
        
(* Trip *)

let trip inf pf outf = 
  let ic = ic_for inf in 
  let write oc i pf a = 
    Rpng.output oc ?gamma:i.Rpng.gamma ?res:i.Rpng.resolution 
      ~w:i.Rpng.width ~h:i.Rpng.height pf a;
  in
  let read_write i outf pf k = 
    let len = Rpng.data_length i.Rpng.width i.Rpng.height pf in
    let a = Array1.create k c_layout len in
    Rpng.input i pf a;
    match outf with
    | None -> if inf <> "" then log "%s" (inf ^ " read.")
    | Some f ->
        try
          let oc = if f <> "" then open_out_bin f else stdout in
          apply (write oc i pf) a ~finally:close_out oc
        with
        | (Sys_error e | Rpng.Error e | Invalid_argument e) -> log_error f e
  in  
  try 
    let i = Rpng.input_info ic in
    match pf with
    | None ->
	begin match i.Rpng.comps with
        | `L `C8 -> read_write i outf Rpng.l_c8 int8_unsigned
        | `L `C16 -> read_write i outf Rpng.l_c16 int16_unsigned
        | `LA `C8 -> read_write i outf Rpng.la_c8 int8_unsigned
        | `LA `C16 -> read_write i outf Rpng.la_c16 int16_unsigned
        | `RGB `C8 -> read_write i outf Rpng.rgb_c8 int8_unsigned
        | `RGB `C16 -> read_write i outf Rpng.rgb_c16 int16_unsigned
        | `RGBA `C8 -> read_write i outf Rpng.rgba_c8 int8_unsigned
        | `RGBA `C16 -> read_write i outf Rpng.rgba_c16 int16_unsigned
        end
    | Some pf -> 
        begin match pf with 
        | `L8 -> read_write i outf Rpng.l_c8 int8_unsigned
        | `L16 -> read_write i outf Rpng.l_c16 int16_unsigned
        | `LA8 -> read_write i outf Rpng.la_c8 int8_unsigned
        | `LA16 -> read_write i outf Rpng.la_c16 int16_unsigned
        | `RGB8 -> read_write i outf Rpng.rgb_c8 int8_unsigned
        | `RGB16 -> read_write i outf Rpng.rgb_c16 int16_unsigned
        | `RGBA8 -> read_write i outf Rpng.rgba_c8 int8_unsigned
        | `RGBA16 -> read_write i outf Rpng.rgba_c16 int16_unsigned
        | `RGBAp8888 -> read_write i outf Rpng.rgba_p8888 int32
        | `ARGBp8888 -> read_write i outf Rpng.argb_p8888 int32
        end
  with
  | (Sys_error e | Rpng.Error e) -> log_error inf e
  | End_of_file -> log_error inf "unexpected end of file"

let pixel_format_of_string = function 
| "L8" -> Some `L8
| "L16" -> Some `L16
| "LA8" -> Some `LA8
| "LA16" -> Some `LA16
| "RGB8" -> Some `RGB8
| "RGB16" -> Some `RGB16
| "RGBA8" -> Some `RGBA8
| "RGBA16" -> Some `RGBA16
| "RGBAp8888" -> Some `RGBAp8888
| "ARGBp8888" -> Some `ARGBp8888
| _ -> None

let main () = 
  let usage = str 
    "Usage: %s [OPTION]... [INFILE]\n\
     \ Recode a PNG from stdin to stdout (image data only).\n\
     Options:" exec
  in
  let cmd = ref `Trip in 
  let set_cmd v () = cmd := v in
  let inf = ref "-" in 
  let set_inf f = 
    if !inf <> "-" then raise (Arg.Bad "only one file can be specified") else
    inf := f
  in
  let pf = ref None in 
  let set_pf e = match pixel_format_of_string e with 
  | None -> log "unsupported pixel format '%s', using default input format" e
  | Some _ as p -> pf := p
  in
  let options = [
    "-i", Arg.Unit (set_cmd `Info), " Only dump image information";
    "-dec", Arg.Unit (set_cmd `Decode), " Decode only, no encoding";
(*    "-enc", Arg.Unit (set_cmd `Encode), 
       " Encode only (random), no decoding";*)
    "-pf", Arg.String set_pf, 
    " Internal and output pixel format: L8, L16, LA8, LA16, RGB8, RGB16, \
      RGBA8, RGBA16, RGBAp8888, ARGBp8888"
  ]
  in
  Arg.parse (Arg.align options) set_inf usage; 
  match !cmd with 
  | `Info -> info !inf 
  | `Decode -> trip !inf !pf None
  | `Trip -> trip !inf !pf (Some "")

let () = main ()

(*---------------------------------------------------------------------------
   Copyright %%COPYRIGHT%%
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
