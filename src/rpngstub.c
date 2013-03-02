
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/signals.h>
#include <caml/bigarray.h>


#define _err_str(s) #s
#define _mlpng_err_str(s,file,line) file ":" _err_str(line) ": " s
#define _mlpng_err(s) _mlpng_err_str(s,__FILE__,__LINE__)

#ifdef RPNG_HAS_PNG
#include <errno.h> 
#include <stdio.h>
#include <png.h>
#include <unistd.h>

#define mlpng_read_struct _mlpng_err("unable to create png read struct")
#define mlpng_write_struct _mlpng_err("unable to create png write struct")
#define mlpng_info_struct _mlpng_err("unable to create png info struct")
#define mlpng_invalid_color_type _mlpng_err("invalid components type")
#define mlpng_invalid_unit_type _mlpng_err("invalid resolution unit type")
#define mlpng_internal _mlpng_err("internal error")

#define Channel_val(v) (*((void **) (Data_custom_val(v))))

/* Components layout, keep in sync with png.ml */
enum { L = 0, LA, RGB, RGBA, ARGB };

typedef struct {
  int reader;
  png_structp png;
  png_infop info;
  png_byte **row_ptrs;
} codec;


CAMLexport int caml_really_getblock(void *, char *, intnat);
CAMLextern void caml_really_putblock (void *, char *, intnat);

void mlpng_pngread_fun (png_structp png, png_bytep data, png_size_t length)
{ caml_really_getblock (png_get_io_ptr (png), (char *) data, length); }

void mlpng_pngwrite_fun (png_structp png, png_bytep data, png_size_t length)
{ caml_really_putblock (png_get_io_ptr (png), (char *) data, length); }

void mlpng_pngerr_fn (png_structp png_ptr, png_const_charp error_msg)
{ caml_invalid_argument ((char *) error_msg); }

void mlpng_pngwarn_fn (png_structp png_ptr, png_const_charp warning_msg)
{ /* silence */; }

void mlpng_finalize_codec (value v) 
{ 
  codec *c = (codec *) Field (v, 1); 

  if (c->reader) 
    png_destroy_read_struct (&c->png, &c->info, NULL);
  else 
    png_destroy_write_struct (&c->png, &c->info);
  if (c->row_ptrs) free (c->row_ptrs); 
  free (c); 
}

CAMLprim value mlpng_codec (value reader)
{ 
  codec *c;
  value v;
  
  if (!(c = malloc (sizeof (codec)))) caml_raise_out_of_memory ();
  c->reader =  Bool_val (reader);
  c->row_ptrs = NULL;
  
  if (c->reader) 
    c->png = png_create_read_struct (PNG_LIBPNG_VER_STRING, NULL,
				     mlpng_pngerr_fn, mlpng_pngwarn_fn);
  else
    c->png = png_create_write_struct (PNG_LIBPNG_VER_STRING, NULL,
				      mlpng_pngerr_fn, mlpng_pngwarn_fn);
  if (!c->png) {
    free (c); 
    caml_failwith (mlpng_read_struct); 
  }
  
  if (!(c->info = png_create_info_struct (c->png))) {
    if (c->reader)
      png_destroy_read_struct (&c->png, NULL, NULL); 
    else
      png_destroy_write_struct (&c->png, NULL);
    free (c); 
    caml_failwith (mlpng_info_struct); 
  }
   
  v = caml_alloc_final (1, mlpng_finalize_codec, 1, 1000);
  Store_field(v, 1, (value) c);
  return v; 
}

CAMLprim value mlpng_input_info (value v, value chan)
{
  CAMLparam2(v, chan);
  CAMLlocal4(t, res, resbox, gbox);
  codec *c = (codec *) Field (v, 1);
  void *channel = Channel_val (chan);
  png_uint_32 resw, resh;
  int resu;
  double gamma;
  int alpha;

  png_set_read_fn(c->png, channel, mlpng_pngread_fun);
  png_read_info (c->png, c->info);

  t = caml_alloc_tuple (6);
  Store_field (t, 0, Val_int (png_get_image_width (c->png, c->info)));
  Store_field (t, 1, Val_int (png_get_image_height (c->png, c->info)));

  alpha = png_get_valid (c->png, c->info, PNG_INFO_tRNS);
  switch (png_get_color_type (c->png, c->info)) {
  case PNG_COLOR_TYPE_GRAY:
    Store_field (t, 2, (alpha) ? Val_int (LA) : Val_int (L)); break;
  case PNG_COLOR_TYPE_GRAY_ALPHA: 
    Store_field (t, 2, Val_int (LA)); break;
  case PNG_COLOR_TYPE_PALETTE:
  case PNG_COLOR_TYPE_RGB:
    Store_field (t, 2, (alpha) ? Val_int (RGBA) : Val_int (RGB)); break;
  case PNG_COLOR_TYPE_RGB_ALPHA: 
    Store_field (t, 2, Val_int (RGBA)); break;
  default:
    caml_failwith (mlpng_invalid_color_type); break;
  }  

  Store_field (t, 3, Val_int (png_get_bit_depth (c->png, c->info)));

  if (png_get_valid (c->png, c->info, PNG_INFO_pHYs)) {
    png_get_pHYs (c->png, c->info, &resw, &resh, &resu);
    switch (resu) {
    case PNG_RESOLUTION_UNKNOWN: resu = 0; break;
    case PNG_RESOLUTION_METER: resu = 1; break; 
    default: caml_failwith (mlpng_invalid_unit_type); break;
    }
    res = caml_alloc_tuple (3);
    Store_field (res, 0, Val_int (resw));
    Store_field (res, 1, Val_int (resh));
    Store_field (res, 2, Val_int (resu));
    resbox = caml_alloc (1, 0);
    Store_field (resbox, 0, res); /* Some (resw, resh, resu) */
    Store_field (t, 4, resbox);
  } 
  else
    Store_field (t, 4, Val_int (0) /* None */);

  if (png_get_valid (c->png, c->info, PNG_INFO_gAMA)) {
    png_get_gAMA (c->png, c->info, &gamma);
    gbox = caml_alloc (1, 0);
    Store_field (gbox, 0, caml_copy_double (gamma)); /* Some gamma */
    Store_field (t, 5, gbox);
  } 
  else
    Store_field (t, 5, Val_int (0) /* None */);

  CAMLreturn(t);
}


CAMLprim value mlpng_set_input_trans (value v, value sg, 
				      value src_pc, value src_pcbytes,
				      value dst_pc, value dst_pcbytes,
				      value packed)
{
  codec *c = (codec *) Field (v, 1);
  double fg;
  int src_rgb, dst_rgb, src_a, dst_a, dst_argb;
  
  if (Int_val (src_pc) > LA) src_rgb = 1; else src_rgb = 0;
  if (Int_val (src_pc) == L || 
      Int_val (src_pc) == RGB) src_a = 0; else src_a = 1;
  if (Int_val (dst_pc) > LA) dst_rgb = 1; else dst_rgb = 0;
  if (Int_val (dst_pc) == L
      || Int_val (dst_pc) == RGB) dst_a = 0; else dst_a = 1;
  if (Int_val (dst_pc) == ARGB) dst_argb = 1; else dst_argb = 0;

  /* Expand palette */
  if (png_get_color_type (c->png, c->info) == PNG_COLOR_TYPE_PALETTE)
    png_set_palette_to_rgb (c->png);
  
  /* Expand bit depth smaller than 8 */
  if (png_get_color_type (c->png, c->info) == PNG_COLOR_TYPE_GRAY && 
      png_get_bit_depth (c->png, c->info) < 8) 
    png_set_expand_gray_1_2_4_to_8 (c->png);
  
  /* Component size */
  if (Int_val (src_pcbytes) > Int_val (dst_pcbytes)) png_set_strip_16 (c->png);
#ifdef __LITTLE_ENDIAN__
  if (Int_val (dst_pcbytes) > 1) png_set_swap (c->png);
#endif

  /* Color */
  if (src_rgb && !dst_rgb) png_set_rgb_to_gray_fixed (c->png, 1, -1, -1);
  else if (!src_rgb && dst_rgb) png_set_gray_to_rgb (c->png);

  /* Alpha */
  if (!dst_a) {
    if (src_a) png_set_strip_alpha (c->png);
  } else if (src_a) {
    /* dst_a && src_a */
    if (!(png_get_color_type (c->png, c->info) & PNG_COLOR_MASK_ALPHA)) {
      if (png_get_valid (c->png, c->info, PNG_INFO_tRNS)) 
	png_set_tRNS_to_alpha (c->png);
    }   
#ifdef __LITTLE_ENDIAN__ 
    if (Bool_val (packed)) {
      png_set_bgr (c->png);
      if (dst_argb) png_set_swap_alpha (c->png);
    }
#else
    if (Bool_val (packed))
      if (dst_argb) png_set_swap_alpha (c->png);
#endif
  } else {
    /* dst_a && !src_a */
    if (Bool_val (packed)) {
#ifdef __LITTLE_ENDIAN__ 
      png_set_bgr (c->png);
      png_set_filler (c->png, 0xFFFF, 
		      (dst_argb) ? PNG_FILLER_AFTER : PNG_FILLER_BEFORE);
#else
      png_set_filler (c->png, 0xFFFF,
		      (dst_argb) ? PNG_FILLER_BEFORE : PNG_FILLER_AFTER);
#endif
    } else {
      png_set_filler (c->png, 0xFFFF, PNG_FILLER_AFTER);
    }    
  }

  /* Gamma correction */
  if (Is_block (sg)) {
    if (png_get_gAMA(c->png, c->info, &fg))
      png_set_gamma(c->png, Double_val (sg), fg);
    else
      png_set_gamma(c->png, Double_val (sg), 0.45455);
  }

  return Val_unit;
}

CAMLprim value mlpng_bc_set_input_trans (value *argv, int argn)
{ 
  return mlpng_set_input_trans (argv[0], argv[1], argv[2], argv[3], argv[4], 
				argv[5], argv[6]);
}

CAMLprim value mlpng_input (value v, value offset, value row_bytes,
			    value height, value d)
{
  codec *c = (codec *) Field (v, 1);
  png_byte *data = ((png_byte *) Data_bigarray_val (d)) + Int_val (offset);
  int h = Int_val (height);
  int row;

  png_read_update_info (c->png, c->info);

  if (!(c->row_ptrs = (png_byte **) malloc (h * sizeof (png_byte *))))
      caml_raise_out_of_memory ();

  for (row = 0; row < h; row++)
    c->row_ptrs [h - 1 - row] = data + (row * Int_val (row_bytes));

  png_read_image (c->png, c->row_ptrs);
  png_read_end (c->png, NULL);
  return Val_unit;
}

CAMLprim value mlpng_write (value v, value chan, value compression, value res,
			    value gamma, value pc, value pcbytes, 
			    value packed, value offset, value row_bytes,
			    value width, value height, value d)
{    
  void *channel = Channel_val (chan);
  codec *c = (codec *) Field (v, 1);
  int compr;
  int color_type;
  int argb = 0;
  int bit_depth = Int_val (pcbytes) * 8;
  png_byte *data = ((png_byte *) Data_bigarray_val (d)) + Int_val (offset);
  int h = Int_val (height);
  int row;
  
  png_set_write_fn(c->png, channel, mlpng_pngwrite_fun, NULL);

  switch (Int_val (compression)) {
  case 0: /* No */ compr = 0; break;
  case 1: /* Best_speed */ compr = 1; break;
  case 2: /* Best_compression */ compr = 9; break;
  case 3: /* Default */ compr = -1; break;
  default: caml_failwith (mlpng_internal);
  }
  png_set_compression_level (c->png, compr);
  
  switch (Int_val (pc)) {
  case L: color_type = PNG_COLOR_TYPE_GRAY; break;
  case LA: color_type = PNG_COLOR_TYPE_GRAY_ALPHA; break;
  case RGB: color_type = PNG_COLOR_TYPE_RGB; break;
  case RGBA: color_type = PNG_COLOR_TYPE_RGBA; break; 
  case ARGB: color_type = PNG_COLOR_TYPE_RGBA; argb = 1; break;
  default: caml_failwith (mlpng_invalid_color_type);
  }
  png_set_IHDR (c->png, c->info, Int_val (width), Int_val (height), bit_depth,
		color_type, PNG_INTERLACE_NONE, PNG_COMPRESSION_TYPE_DEFAULT,
		PNG_FILTER_TYPE_DEFAULT);

  if (Is_block (gamma))
    png_set_gAMA (c->png, c->info, Double_val (Field (gamma, 0)));

  if (Is_block (res)) 
    png_set_pHYs (c->png, c->info, 
		  Int_val (Field ( Field (res, 0), 0)),
		  Int_val (Field ( Field (res, 0), 1)),
		  (Int_val (Field ( Field (res, 0), 2))) ?
		   PNG_RESOLUTION_METER : PNG_RESOLUTION_UNKNOWN);

  png_write_info (c->png, c->info);

#ifdef __LITTLE_ENDIAN__
  if (bit_depth > 8) png_set_swap (c->png);
  if (Bool_val (packed)) png_set_bgr (c->png);
  if (argb) png_set_swap_alpha (c->png);
#else
  if (argb) png_set_swap_alpha (c->png);
#endif

  if (!(c->row_ptrs = (png_byte **) malloc (h * sizeof (png_byte *))))
    caml_raise_out_of_memory ();

  for (row = 0; row < h; row++)
    c->row_ptrs [h - 1 - row] = data + (row * Int_val (row_bytes));

  png_write_image (c->png, c->row_ptrs);
  png_write_end (c->png, NULL);  
  return Val_unit;
}

CAMLprim value mlpng_bc_write (value *argv, int argn)
{ 
  return mlpng_write (argv[0], argv[1], argv[2], argv[3], argv[4], 
		      argv[5], argv[6], argv[7], argv[8], argv[9], 
		      argv[10], argv[11], argv[12]);
}

#else

#define mlpng_unimplemented _mlpng_err("lit io compiled without png support")

CAMLprim value mlpng_codec (value reader)
{ caml_failwith (mlpng_unimplemented); }

CAMLprim value mlpng_input_info (value v, value chan)
{ caml_failwith (mlpng_unimplemented); }

CAMLprim value mlpng_set_input_trans (value v, value sg, 
					value src_pc, value src_pcbytes,
					value dst_pc, value dst_pcbytes,
					value packed)
{ caml_failwith (mlpng_unimplemented); }

CAMLprim value mlpng_bc_set_input_trans (value *argv, int argn)
{ caml_failwith (mlpng_unimplemented); }

CAMLprim value mlpng_input (value v, value offset, value row_bytes,
			    value height, value d)
{ caml_failwith (mlpng_unimplemented); }

CAMLprim value mlpng_write (value v, value chan, value compression, value res,
			    value gamma, value pc, value pcbytes, 
			    value packed, value offset, value row_bytes,
			    value width, value height, value d)
{ caml_failwith (mlpng_unimplemented); }

CAMLprim value mlpng_bc_write (value *argv, int argn)
{ caml_failwith (mlpng_unimplemented); }
#endif


