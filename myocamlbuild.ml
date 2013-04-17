open Ocamlbuild_plugin
open Command

let pkg_config flags package = 
  let cmd tmp = 
    Command.execute ~quiet:true & 
    Cmd( S [ A "pkg-config"; A ("--" ^ flags); A package; Sh ">"; A tmp]);
    List.map (fun arg -> A arg) (string_list_of_file tmp)
  in
  with_temp_file "rpng" "pkg-config" cmd
       
let pkg_config_lib has_lib lib = 
  let tag = Printf.sprintf "use_%s" lib in 
  let opts opt l = List.map (fun a -> S [A opt; a]) l in
  let cflags = opts "-ccopt" (has_lib :: pkg_config "cflags" lib) in
  let ldflags = opts "-cclib" (pkg_config "libs" lib) in 
  flag ["c"; "compile"; tag] (S cflags); 
  flag ["link"; "ocaml"; tag] (S ldflags)
;;
let libpng = "libpng"
let has_libpng = A "-DRPNG_HAS_PNG"

let () = 
  dispatch begin function
  | After_rules ->
      pkg_config_lib has_libpng libpng;
      dep [ "link"; "ocaml"; "use_libpng" ] ["src/rpngstub.o"];
  | _ -> ()
end
