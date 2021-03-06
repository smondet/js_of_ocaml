module type S = sig
  type ocaml_text

  type js_text

  type sourcemap_text

  type ocaml_file

  type js_file

  type sourcemap_file

  type cmo_file

  type bc_file

  val read_js : js_file -> js_text

  val read_map : sourcemap_file -> sourcemap_text

  val read_ocaml : ocaml_file -> ocaml_text

  val write_file : string -> string -> unit

  val write_js : name:string -> js_text -> js_file

  val write_ocaml : name:string -> ocaml_text -> ocaml_file

  val js_text_of_string : string -> js_text

  val ocaml_text_of_string : string -> ocaml_text

  val string_of_js_text : js_text -> string

  val string_of_map_text : sourcemap_text -> string

  val string_of_ocaml_text : ocaml_text -> string

  val path_of_ocaml_file : ocaml_file -> string

  val path_of_js_file : js_file -> string

  val path_of_map_file : sourcemap_file -> string

  val path_of_cmo_file : cmo_file -> string

  val path_of_bc_file : bc_file -> string

  val ocaml_file_of_path : string -> ocaml_file

  val js_file_of_path : string -> js_file

  val map_file_of_path : string -> sourcemap_file

  val cmo_file_of_path : string -> cmo_file

  val bc_file_of_path : string -> bc_file
end
