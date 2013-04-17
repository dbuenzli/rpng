(*---------------------------------------------------------------------------
   Copyright %%COPYRIGHT%%. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(** PNG codec. 

   Decodes and encodes PNG streams.

   Reads and writes PNG streams with bigarrays. The first pixel of
   the array is the image's lower left pixel.

   {b Limitations.}
   {ul
   {- {Gg.Raster.format}'s size are [int], PNG ones are [int32]
       this can be a problem on 32bits machines.}
    {- Width, height and resolution values are mapped to [int]'s but
    can be [int32] in practice.}
    {- The writer is not clever, no alpha separation or color indexing.}
    {- No chunk management.}} *)

(** {1 Basic types} *)

exception Error of string
(** Raised on PNG data errors. *)

(** Unit of measurement. *)
type measure = Unknown | Meter
               
type ('a, 'b) data = ('a, 'b, Bigarray.c_layout) Bigarray.Array1.t
(** The type for pixel data.*)

type ('a, 'b) pf
(** The type for pixel formats, see values below. *)

val data_length : ?w_skip:int -> w:int -> h:int -> ('a, 'b) pf -> int
(** Array length needed to store an image. [w_skip] is
    the number of array elements to skip between two consecutive lines,
    defaults to 0. *)

(** Below, cX means X bits per components. *)
val l_c8 : (int, Bigarray.int8_unsigned_elt) pf
val l_c16 : (int, Bigarray.int16_unsigned_elt) pf
val la_c8 : (int, Bigarray.int8_unsigned_elt) pf
val la_c16 : (int, Bigarray.int16_unsigned_elt) pf
val rgb_c8 : (int, Bigarray.int8_unsigned_elt) pf
val rgb_c16 : (int, Bigarray.int16_unsigned_elt) pf
val rgba_c8 : (int, Bigarray.int8_unsigned_elt) pf
val rgba_c16 : (int, Bigarray.int16_unsigned_elt) pf
val rgba_p8888 : (int32, Bigarray.int32_elt) pf
(** Pixel stored as [0xRRGGBBAA]. *)
val argb_p8888 : (int32, Bigarray.int32_elt) pf
(** Pixel stored as [0xAARRGGBB] *)

(** {1:read Reading} *)

type c
type comps = [ 
  | `L of [`C8 | `C16 ] 
  | `LA of [`C8 | `C16 ] 
  | `RGB of [`C8 | `C16 ] 
  | `RGBA of [`C8 | `C16 ] ]  
(** Image components and their size 
    (size smaller than 8 bits are expanded to 8). *)

type info = {
  comps : comps;
  width : int; 
  height : int; 
  resolution : (int * int * measure) option; 
  (** Resolution, horiz. and vert. number of pixels per unit. *)  
  gamma : float option; (** Gamma exponent. *) 
  in_c : in_channel; (** The channel you read from. *)
  c : c; (** Ignore *) }
(** Image info. *)

val input_info : in_channel -> info
(** Reads a PNG magic number and info about the image. 
    Raises {!Png.Error} (data), [Sys_error] and [End_of_file] (io). *)

val input : info -> ?screen_gamma:float -> ?first:int ->
  ?w_skip:int -> ('a, 'b) pf -> ('a, 'b) data -> unit
(** Reads the image data. Pixels are converted
    to the given pixel format. Due to limitations in libpng,
    conversions from component sizes [`C8] to 16 bits pixel formats is
    not supported and raise [Invalid_arg].
    {ul
    {- [screen_gamma], the screen gamma exponent. If unspecified, no gamma 
    correction occurs, if specified but the image has no gamma a default 
    value of [0.45455] is assumed for the file. }
    {- [first], first array element where the data will be written to, 
    defaults to [0].}
    {- [w_skip], number of array elements to skip between two consecutive 
    lines, defaults to [0].}}
    Raises {!Png.Error} (data), [Sys_error] and [End_of_file] (io) and 
    [Invalid_arg] (bounds or unsupported conversion).
*)

(** {1:write Writing} *)

type compression = No | Best_speed | Best_compression | Default
                   
val output : out_channel -> ?compress:compression ->
  ?gamma:float -> ?res:int * int * measure -> ?first:int -> 
  ?w_skip:int -> w:int -> h:int -> ('a, 'b) pf -> 
  ('a, 'b) data -> unit
(** Writes a PNG image on the given channel. 
    {ul
    {- [compress], desired compression level, defaults to [Default].}
    {- [gamma], gamma exponent of the image, defaults to [None].}
    {- [res], image resolution, defaults to [None].}
    {- [first], array element where the image data starts, defaults to [0].}
    {- [w_skip], number of array elements to skip between two 
       consecutive lines, defaults to [0].}
    {- [w], [h], width and height of the image.}}
    Raises {!Png.Error} (data), [Sys_error] (io) and [Invalid_arg] (bounds). *)

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
