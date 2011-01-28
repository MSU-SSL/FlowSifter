open Ocamlbuild_plugin
open Command
open Format

module Util = struct

  (* raises not found if c not in string *)
  let split_first c s =
    let k = String.index s c in
    let n = String.length s in
    String.sub s 0 k, String.sub s (k + 1) (n - k - 1)

  (* raises not found if c not in string *)
  let split_last c s =
    let k = String.rindex s c in
    let n = String.length s in
    String.sub s 0 k, String.sub s (k + 1) (n - k - 1)

  let split c s = 
    let n = String.length s in
    let rec again i lst =
      begin try let k = String.rindex_from s i c in
        again (k - 1) ((if i = k then "" else (String.sub s (k + 1) (i - k))) :: lst)
        with _ -> (String.sub s 0 (i + 1)) :: lst
      end
    in again (n - 1) []

  let prechomp prefix s =
    let k = String.length prefix in
    let n = String.length s in
    try
      if k > n then raise Not_found;
      for i = 0 to k - 1 do
        if prefix.[i] <> s.[i] then raise Not_found
      done;
      if k = n then "" else String.sub s k (n - k)
    with Not_found -> s

  let chomp suffix s =
    let k = String.length suffix in
    let n = String.length s in
    try
      if k > n then raise Not_found;
      for i = 0 to k - 1 do
        if suffix.[i] <> s.[n - k + i] then raise Not_found
      done;
      if k = n then "" else String.sub s 0 (n - k)      
    with Not_found -> s

  (* generic value type for navigating variant values - derived from JSON types
     but useful in other contexts also *)
  module Value = struct
    type value_type =
      Object of (string * value_type) list
    | Array of value_type list
    | String of string
    | Int of int
    | Float of float
    | Bool of bool
    | Null
  end

end (* end of Util module *)

module FileSet = struct
    include Set.Make(String)
    
    let of_list l = List.fold_right add l empty;;
    let to_list s = fold begin fun elt l -> elt :: l end s [];;
    let to_string s = 
      fold (fun elt str -> if str <> "" then str ^ " " ^ elt else elt) s "";;

    let print s f =
      ignore (fold begin fun elt first ->
        if not first then output_string f " ";
        output_string f elt;
        false
      end s true);;

    let add_list s l =
        let s2 = of_list l in union s s2;;

    let size = cardinal;;
    
    let write s filename = with_output_file filename (print s);;

end;;

module ReadProcess = struct
  (* based on PLEAC code *)

  let read_process command =
    let buffer_size = 2048 in
    let buffer = Buffer.create buffer_size in
    let string = String.create buffer_size in
    let in_channel = Unix.open_process_in command in
    let chars_read = ref 1 in
    while !chars_read <> 0 do
      chars_read := input in_channel string 0 buffer_size;
      Buffer.add_substring buffer string 0 !chars_read
    done;
    ignore (Unix.close_process_in in_channel);
    Buffer.contents buffer;;

  let read_process_line command =
    let in_channel = Unix.open_process_in command in
    let line = try input_line in_channel with End_of_file -> "" in
    ignore (Unix.close_process_in in_channel);
    line

  let read_process_lines command =
    let lines = ref [] in
    let in_channel = Unix.open_process_in command in
    begin
      try
        while true do
          lines := input_line in_channel :: !lines
        done;
      with End_of_file ->
        ignore (Unix.close_process_in in_channel)
    end;
    List.rev !lines;;
    
end;;

module MyPath = struct

  module PathnameX = struct
    (* partial reimplementation because we don't have access to the internals of Pathname *)
    open Ocamlbuild_plugin.Pathname
    open Operators
  
    let dir_seps = ['/';'\\']
  
    let split p =
      let rec go p acc =
        let dir = dirname p in
        if dir = p then dir, acc
        else go dir (basename p :: acc)
      in go p []
  
    let join root paths =
      let root = if root = current_dir_name then "" else root in
      List.fold_left (/) root paths
  
    let is_prefix x y =
      let string_before s pos = String.sub s 0 pos in
      let lx = String.length x and ly = String.length y in
      if lx = ly then x = (string_before y lx)
      else if lx < ly then x = (string_before y lx) && List.mem y.[lx] dir_seps
      else false
  
  end (* end of PathnameX module *)

  open PathnameX (* access to non-public Pathname functions *)
  include Ocamlbuild_plugin.Pathname
  open Operators
  
  let mkdir dir =
    let cmd = "mkdir \"" ^ dir ^ "\"" in
    if 0 <> Sys.command cmd then failwith cmd
  
  let rec mkdir_p dir =
    if exists dir then ()
    else (mkdir_p (Filename.dirname dir); mkdir dir)
  
  let is_prefix x y = is_prefix x y
  let is_absolute x = String.length x > 0 && List.mem x.[0] dir_seps
  let is_relative x = not (is_absolute x)
  let is_current x = x = current_dir_name
  
  let absolute x y = if is_absolute x then x else concat y x
  
  (* relative path from y to x - use relative_at when resulting path needs to go outside y *)
  let relative x y =
    let rx, lx = split x
    and ry, ly = split y in
    let rec aux = function
    | hx::tx, hy::ty when hx = hy -> aux (tx, ty)
    | lx, [] -> lx, []
    | lx, hy :: ty -> aux ((".." :: lx), ty)
    in let paths = fst (aux (lx, ly)) in
    join current_dir_name paths

  (* relative path from y to x where both x and y are relative to base
     this handles the case where y is not a parent of x *)
  let relative_in x y base =
    relative (base / x) (base / y)

  (* Returns list of lists of paths where the inner list is valid prefix paths appended to one suffix path
     this is useful for adding include directories to targets for the ocamlbuild build function.
     the map function is applied to each suffix to handle OS specific issues like .a -> .lib
     Any resulting path that cannot be normalized is silently removed *)
  let translate prefixes suffixes map =
    (*  Append suffix to all given prefixes and remove any results that are not valid. *)
    let append_prefixes prefixes suffix =
      List.fold_right
        (fun prefix acc -> try normalize (prefix / suffix) :: acc with _ -> acc)
        prefixes [] in
    List.map (fun suffix -> append_prefixes prefixes (map suffix)) suffixes;;

end (* end of MyPath module *)

module MyStringUtil = struct

  (*  Returns index of first occurence of a char in charset.
      Raises Not_found if string does not contain any char in charset *)
  let find_charset charset path first last =
    let rec aux offset =
      if offset >= last then
        raise Not_found
      else
        try ignore (String.index charset path.[offset]); (offset, 1)
        with Not_found -> aux (offset + 1)
      in
      aux first
        
  (* f returns first split position and the separator width which can be zero
     if f raises Not_found or returns (0, 0) the reminder of the string becomes last element *)
  let split_string f s =
    let last = String.length s in
    let first = 0 in
    let rec aux offset acc = 
      try
        let pos, w = f s offset last in
        if pos + w <= offset then raise Not_found;
        let acc = (String.sub s offset (pos - offset)) :: acc in
          aux (pos + w) acc
      with Not_found -> (String.sub s offset (last - offset)) :: acc
    in List.rev (aux first [])
  
  let split_string_with seps s = split_string (fun s' offset -> find_charset seps s' offset) s

end

module Platform = struct


  let windows = match Sys.os_type with "Win32" | "Win64" | "MinGW" -> true | _ -> false;;
  let cygwin = Sys.os_type = "Cygwin";;
  let mingw = Sys.os_type = "MinGW";;

  let platform_tags = match Sys.os_type with
    | "Win32"  -> ["windows"; "win32"]
    | "Win64"  -> ["windows"; "win64"]
    | "MinGW"  -> ["windows"; "win32"; "mingw"]
    | "Cygwin" -> ["windows"; "win32"; "cygwin"]
    | _        -> ["posix"; "unix"];;

  let find_ocamllib () = ReadProcess.read_process_line "ocamlc -where";;

  (* ocamlbuild doesn't like empty extension - can result in infite loops if no valid target exists *)
  let ext_prog = if windows then "exe" else "prog" (* "" *) ;;
  
  let ctool = if windows && not mingw then "msvc" else "gnu";;
  
  let cc         = match ctool with "mscv" -> Sh "cl"  | _ -> Sh "gcc";;
  let cc_out     = match ctool with "msvc" -> A "/Fo"  | _ -> A  "-o";;
  let cc_compile = match ctool with "msvc" -> A "/c" | _ -> A  "-c";;
  let cxx        = match ctool with "mscv" -> Sh "cl"  | _ -> Sh "g++";;
  let cpp        = Sh "ucpp";; (* standalone c preprocessor tool *)

  let lib_cmd dst objs tags = match ctool with
    | "msvc"    -> Cmd(S[Sh "lib"; T tags; atomize objs; A"/OUT:"; Px dst;])
    | "gnu" | _ -> Cmd(S[Sh "ar"; A"rcs"; Px dst; T tags; atomize objs]);;
  
  let cppi = Sh "cppi";; (* generate list of directly included files, ignoring preprocessor flags *)
  
  let ext_obj = if windows && ctool <> "gnu" then "obj" else "o";;
  let ext_lib = if windows && ctool <> "gnu" then "lib" else "a";;
  let ext_dll = if windows && ctool <> "gnu" then "dll" else "so";;

  let redirect_out = Sh ">";;
  let pipe = Sh "|";;
  let keep_dir = Sh "&&";;
  
  let ragel = Sh "ragel";;  
      
  (* specification files like .clib and .cprog are normally user generated, under source control and cross platform
     this mapping translates generic object names etc. to platform specific names during the build process.
     It also adds conveniences such as specifying source files instead of object files
     for convenience, improved platform neutrality and to quickly generate spec files by typing
     "ls *.c > mylib.clib" or "ls *.c > myprog.cprog"
   *)
  let map_target_ext name =
    let map = begin function
    | "opt"                  -> "native"
    | "so"   | "dll"         -> ext_dll
    | "a"    | "lib"         -> ext_lib
    | "o"    | "obj"         -> ext_obj
    | "prog" | "" | "exe"    -> ext_prog
    | x                      -> x end in
    let ext = map (try Pathname.get_extension name with _ -> "") in
    let base = try Pathname.remove_extension name with _ -> name in
    if ext = "" then base else base ^ "." ^ ext;;
  
  (* needed by OS-X C headers in preprocessor since ucpp does not define it with -Y switch *)
  let le = A"-D__LITTLE_ENDIAN__";;
  let be = A"-D__BIG_ENDIAN__";;
  let endian = le;; (* TODO: figure out how to detect this without using a C extension ... *)

  (* use a C pre-processor to generated dependencies - often not needed, slower and less portable *)
  let use_cpp_depends = false;;
  
  let debug_build = false;;
  
end;;

module C = Platform



module Build_tools = struct
  
  let good_results results = List.filter (function Outcome.Good x -> true | Outcome.Bad x -> false) results;;

  let good_includes (names, results) =
    List.fold_right2 (fun name result l -> match result with Outcome.Good x -> name :: l | _ -> l) names results []

  let debug_fileset src fs =
    output_string stdout (sprintf "%s: %s\n" src (FileSet.to_string fs));;
      
  let debug_string_list msg token_list =
      output_string stdout (msg ^ "\n");
      output_string stdout ((String.concat "/" token_list) ^ "\n");;
  
  let debug_string msg str =
      output_string stdout (msg ^ ": " ^ str ^ "\n");;

  let debug_results src (names, results) =
    let gi = good_includes (names, results) in
    let fs = FileSet.of_list gi in
    output_string stdout (sprintf "%s\n  name count %i, result count %i\n" src (List.length names) (List.length results));
    output_string stdout (sprintf "  good includes count %i, file set count %i\n" (List.length gi) (FileSet.size fs));
    output_string stdout (sprintf "  file set: %s\n" (FileSet.to_string fs));;
  

  let is_tag_pattern token =
    (token <> "") && (token.[0] = '[') && (token.[(String.length token) - 1] = ']');;

  let is_inclusive_tag tag =
    (tag = "") or (tag.[0] <> '~');;

  (*  Parses tag pattern and return a list of tags.
      Remove [] delimiters and split content at ',' *)
  let parse_tag_pattern token =
    MyStringUtil.split_string_with "," (String.sub token 1 ((String.length token) - 2));;

  (* Takes a single list of tags and returns a pair of tag sets;
      one inclusive and one exclusive. Removes empty tags.
  *)
  let tags_pair_of_list tag_list =
    let incl, excl = List.partition is_inclusive_tag tag_list in
    let excl = List.map (fun s -> String.sub s 1 ((String.length s) - 1)) excl in (* remove '~' *)
    Tags.remove "" (Tags.of_list incl), Tags.remove "" (Tags.of_list excl);;

  let does_match_tags_pair tags (incl, excl) =
    (Tags.subset incl tags) && (Tags.is_empty (Tags.inter excl tags));;

  let does_match_tag_list tags tag_list =
      does_match_tags_pair tags (tags_pair_of_list tag_list);;

  (* Given a list of tokens which are targets or tag patterns:
     Group each tag pattern with the targets that follow.
     Each group is represent as a pair of lists: tags and targets. 
     Return a list of these groups.
     The first pattern defaults to an empty tag list that always matches. *)
  let parse_conditional_targets token_list =
      let pattern', targets', result' = List.fold_left
      begin fun (tag_list, targets, result) token ->
        if is_tag_pattern token
        then (parse_tag_pattern token, [], (tag_list, List.rev targets) :: result)
        else (tag_list, token :: targets, result)
      end ([], [], []) token_list in
      List.rev ((pattern', List.rev targets') :: result');;

(* start of exclusive tags in flags:
    
  Modify ocamlbuild tag_handler to interpret exclusive tags like "~debug".
  It is possible to update the ocamlbuild tag handler,
  but since we cannot access Flags.get_flags (), we have to store our own flags.
*)
let all_my_flags = ref [];;

let my_flag tags flags =
  all_my_flags := (tags_pair_of_list tags, flags) :: !all_my_flags;;

let my_flags_of_tags old_flags tags =
  S begin
    List.fold_left begin fun acc (xtags_pair, xflags) ->
      if does_match_tags_pair tags xtags_pair then xflags :: acc
      else acc
    end [old_flags] (!all_my_flags)
  end;;

(* this is the only way to get to the old flags - so we can our own to these *)
let old_tag_handler = !Command.tag_handler;;

let my_tag_handler tags =
  my_flags_of_tags (old_tag_handler tags) tags;;

let () = Command.tag_handler := my_tag_handler;;

let flag = my_flag;;

(* end of exclusive tags in flags *)


  let append_before_ext path s =
    let base = try Pathname.remove_extension path with _ -> path in
    let ext = try "." ^ (Pathname.get_extension path) with _ -> "" in
    base ^ s ^ ext;;

  let map_variant variant path =
    append_before_ext path (".variant_" ^ variant);;

  (* get_variant ["debug"; "trace"] "src/lib/mylib.a"
     -> "src/lib/mylib.variant_debug,trace.a" *)
  let get_variant variant_tags path =
    map_variant (String.concat "," variant_tags) path;;
 
  let build_target_list names src map env build =
    let include_dirs = Pathname.include_dirs_of (Pathname.dirname src) in
    let map =
      try let v = env "%(variant)" in
        (fun name -> map (map_variant v name))
      with _ -> map in
    let targets = MyPath.translate include_dirs names map in
    names, (build targets);;


  let build_conditional_target_list tags tagged_targets map env build =
    let targets' =
      List.flatten
      (List.map snd
        (List.filter (fun (tag_list, targets) -> does_match_tag_list tags tag_list) tagged_targets))    
    in
    build_target_list targets' map env build;;

  let build_targets src map env build =
    build_target_list (string_list_of_file src) src map env build;;

  (*  Read an external file 'srcfile' and build all specified files
      translate the extension in the file to the equivalent platform specific extension
      expand the filename to all include directories as potential build targets.
      If the extension is not recognized it is used as is.
     
      The returned list of targets actually build is typically used to link libraries and programs,
      but can be used by any consumer of multiple files. *)
  let build_target_spec src env build =
    List.map Outcome.good (snd (build_targets src C.map_target_ext env build));;

  (* Like build_targets, but the input src file may contain embedded tag patterns
     that are matched against the given tags. Only matched objects will be built. *)
  let build_conditional_targets tags src map env build =
     build_conditional_target_list tags (parse_conditional_targets (string_list_of_file src)) src map env build;;
  
  let build_conditional_target_spec tags src env build =
    List.map Outcome.good (snd (build_conditional_targets tags src C.map_target_ext env build));;

  let accumulate_targets src names =
    let include_dirs = Pathname.include_dirs_of (Pathname.dirname src) in
    let map name = name-.-"includes_acc" in
    let targets = MyPath.translate include_dirs names map in
    List.fold_right begin fun candidates fs -> (* candidates are the different search paths for src.includes_acc *)
      FileSet.add_list fs (string_list_of_file (List.find Pathname.exists candidates)) end 
      targets FileSet.empty;;

  (* 
      Include paths and dependency scanning:
      
      If a file located in "myproject" includes another
      file, for example: #include <somelib.h>, where mylib.h is located in external/include/somelib.h,
      then the path to myinclude must be added to the set of include directories "include_dirs_of" using 
      Pathname.define_context "myproject" ["external/include"];
      If this is done, build_targets will try both
        "myproject/mylib.h"
      and
        "external/include/mylib.h"
      The last one will succeed assuming define_context was set correctly.
      (If define_context is not set, the build engine fails to locate the file, and it is
       assumed to be a false positive which is removed from the set of include files
       to depend upon - this will likely cause the compile step to fail later on.).
      
   *)
  let build_includes src env build =
    let map name = name-.-"includes_acc" in      
    FileSet.of_list (good_includes (build_targets (src-.-"includes") map env build))

  let build_depends src env build =
    let map name = name in
    ignore (build_targets (src-.-"depends") map env build);;

  (*  Generic simple dependency scanner to create .includes files automatically.
      Mostly for .h, .c and .cpp files but often can also work in other cases.
      For example a parser tool that includes an .h file - the actual parser include statement is missed
      but because C/C++ code is embedded which included the same file, the correct dependency is created. *)
  let mk_includes src env build =
    let src = env src in
    let dst = src-.-"includes" in
    Cmd(S[C.cppi; T(tags_of_pathname src++"c"++"c_cxx"++"include"++C.ctool); A src; A"-o"; Px dst]);;

  let accumulated_include_rule name ~prod ~deps =
    let mk_acc_includes env build =
      let src = env "%" in
      let dst = env prod in
      let fs = build_includes src env build in
      if C.debug_build then debug_fileset src fs;
      let fs_child = accumulate_targets src (FileSet.to_list fs) in
      let fs = FileSet.union fs fs_child in
      let text = FileSet.to_string fs in
      Echo([text], dst)
    in
    rule name ~prod ~deps mk_acc_includes;;

 
  let mk_depends src env build =
    let src = env src in
    cp (src-.-"includes_acc") (src-.-"depends");;

  let mk_c_depends src env build =
    let src = env src in
    let dst = src-.-"depends" in
    Cmd(S[C.cpp; T(tags_of_pathname src++"c"++"c_cxx"++"depend"++C.ctool); A src; C.cc_out; Px dst;
      (*  pre-processor sometimes invent errors *)
      Sh "& echo ignore return code"]);;

  let mk_cxx_depends src env build =
    let src = env src in
    let dst = src-.-"depends" in    
    Cmd(S[C.cpp; T(tags_of_pathname src++"cxx"++"c_cxx"++"depend"++C.ctool);
      A src; C.cc_out; Px dst;
      Sh "& echo ignore return code"]);;

  let get_tags src dst env =
    let v = try env "%(variant)" with _ -> "default" in
    let variant_tags = Tags.of_list (MyStringUtil.split_string_with "," v) in
    let common_tags = tags_of_pathname src in
    let tags = Tags.union common_tags (tags_of_pathname dst) in
    Tags.union tags variant_tags

  let mk_cobj src dst env build =
    let src = env src and dst = env dst in
    build_depends src env build;
    let tags = (get_tags src dst env)++"c"++"c_cxx"++"compile"++C.ctool in
    Cmd(S[C.cc; T tags; C.cc_compile; A src; C.cc_out; Px dst]);;

  let mk_cxxobj src dst env build =
    let src = env src and dst = env dst in
    build_depends src env build;
    let tags = (get_tags src dst env)++"cxx"++"c_cxx"++"compile"++C.ctool in
    Cmd(S[C.cxx; T tags; C.cc_compile; A src; C.cc_out; Px dst]);;

  let mk_cprog src dst env build =
    let src = env src and dst = env dst in
    let tags = (get_tags src dst env)++"c"++"c_cxx"++"program"++"link"++C.ctool in
    let objs = build_conditional_target_spec tags src env build in
    Cmd(S[C.cc; C.cc_out; Px dst; atomize objs; T tags]);;

  let mk_component src dst env build =
    let src = env src and dst = env dst in
    let tags = (get_tags src dst env)++"component"++C.ctool in
    ignore (build_conditional_target_spec tags src env build); Nop;;

  let mk_cxxprog src dst env build =
    let src = env src and dst = env dst in
    let tags = (get_tags src dst env)++"cxx"++"c_cxx"++"program"++"link"++C.ctool in
    let objs = build_conditional_target_spec tags src env build in
    Cmd(S[C.cxx; C.cc_out; Px dst; atomize objs; T tags]);;

  (* both for c and cxx (C++) hence c and cxx tags are set *)
  let mk_clib src dst env build =
    let dst = env dst and src = env src in
    let tags = (get_tags src dst env)++"c"++"cxx"++"c_cxx"++"library"++"link"++C.ctool in
    let objs = build_conditional_target_spec tags src env build in 
    C.lib_cmd dst objs tags;;

  let mk_rl_c src dst env build =
    let src = env src and dst = env dst in
    build_depends src env build;
    (* ragel does not include files relative to src path *)
    let path = (Pathname.dirname src) in
    let relsrc = Pathname.basename src in
    let reldst = Pathname.basename dst in
    Cmd(S[
      Sh "cd"; A path; C.keep_dir;
      (* as of Ragel 6.0, before pipe to backend code gen. was needed *)
      C.ragel; A relsrc; T(tags_of_pathname src++"ragel"++"compile"++"rl"); A "-C"; A"-o"; Px reldst]);;

  let naming_copy_rule ?insert src dst = copy_rule (sprintf "Copy %s -> %s" src dst) ?insert src dst;;
  let naming_map_copy_rule ?insert src dst = copy_rule (sprintf "Map Ext Copy %s -> %s" src dst) ?insert (C.map_target_ext src) (C.map_target_ext dst);;

  let apply_project project = dispatch
  begin function
    | Before_options -> ()
    | After_options ->  ()
    | Before_rules ->
      
 
      (* before, to override ocaml c -> o *)    
        
      let src, dst = "%.c", "%." ^ C.ext_obj in
      rule "c compile" ~prod:dst ~deps:[src ^ ".depends"; src]
        (mk_cobj src dst);

      let src, dst =  "%.c", "%.variant_%(variant)." ^ C.ext_obj in
      rule "c compile variant" ~prod:dst ~deps:[src ^ ".depends"; src]
        (mk_cobj src dst);

      let src, dst =  "%.cxx", "%." ^ C.ext_obj in
      rule "cxx compile" ~prod:dst ~deps:[src ^ ".depends"; src]
        (mk_cxxobj src dst);

      let src, dst =  "%.cpp", "%." ^ C.ext_obj in
      rule "cpp compile" ~prod:dst ~deps:[src ^ ".depends"; src]
        (mk_cxxobj src dst);

      let src, dst =  "%.cxx", "%.variant_%(variant)." ^ C.ext_obj in
      rule "cxx compile variant" ~prod:dst ~deps:[src ^ ".depends"; src]
        (mk_cxxobj src dst);

      let src, dst =  "%.cpp", "%.variant_%(variant)." ^ C.ext_obj in
      rule "cpp compile variant" ~prod:dst ~deps:[src ^ ".depends"; src]
        (mk_cxxobj src dst);

      (* preprocessor does not support variants *)
      if C.use_cpp_depends then begin
        rule "x preprocessor depends" ~prod:"%.c.depends" ~dep:"%.c.includes_acc"
          (mk_c_depends "%.c");

        rule "cxx preprocessor depends" ~prod:"%.cxx.depends" ~dep:"%.cxx.includes_acc"
          (mk_cxx_depends "%.cxx");

        rule "cpp preprocessor depends" ~prod:"%.cpp.depends" ~dep:"%.cpp.includes_acc"
          (mk_cxx_depends "%.cpp");
      end else (); (* use default includes and depends rules *)

      let src, dst = "%.cprog", "%.prog" in
      rule "c program" ~prod:dst ~dep:src
        (mk_cprog src dst);

      let src, dst = "%.cprog", "%.variant_%(variant).prog" in
      rule "c program variant" ~prod:dst ~dep:src
        (mk_cprog src dst);
      
      let src, dst = "%.clib", "%." ^ C.ext_lib in
      rule "c/cxx library"   ~prod:dst ~dep:src
        (mk_clib src dst);

      let src, dst = "%.clib", "%.variant_%(variant)." ^ C.ext_lib in   
      rule "c/cxx library variant"   ~prod:dst ~dep:src
        (mk_clib src dst);

      let src, dst = "%.cxx_prog", "%.prog" in
      rule "cxx program" ~prod:dst ~dep:src
        (mk_cxxprog src dst);

      let src, dst = "%.cpp_prog", "%.prog" in
      rule "cpp program" ~prod:dst ~dep:src
        (mk_cxxprog src dst);

      let src, dst = "%.cxx_prog", "%.variant_%(variant).prog" in
      rule "cxx program variant" ~prod:dst ~dep:src
        (mk_cxxprog src dst);

      let src, dst = "%.cpp_prog", "%.variant_%(variant).prog" in
      rule "cpp program variant" ~prod:dst ~dep:src
        (mk_cxxprog src dst);

      (* Auto-generate cprog from source file such that for example
         hello.prog can be built from hello.c via hello.cprog even if hello.cprog is missing.
         This rule would then write "hello.o" into a new hello.cprog file.
         (We could also call the c-compiler directly, but here we get build variant magic for free,
          such as the targets hello.variant_release.prog and hello.variant_debug.prog.
         Normally .cprog files are user-specified and contain multiple object files.
       *)


      let src, dst = "%.c", "%.cprog" in
      rule "auto-generate cprog file from single source file name" ~prod:dst ~dep:src
        (fun env build -> Echo([Pathname.update_extension "o" (Pathname.basename (env src))], env dst));
   
      let src, dst = "%.cxx", "%.cxx_prog" in
      rule "auto-generate cxx_prog file (C++ program) from single .cxx file" ~prod:dst ~dep:src
        (fun env build -> Echo([Pathname.update_extension "o" (Pathname.basename (env src))], env dst));

      let src, dst = "%.cpp", "%.cpp_prog" in
      rule "auto-generate cpp_prog file (C++ program) from single .cpp file" ~prod:dst ~dep:src
        (fun env build -> Echo([Pathname.update_extension "o" (Pathname.basename (env src))], env dst));

      let src, dst = "%." ^ C.ext_obj, "%.clib" in
      rule "auto-generate clib file from single object file name" ~prod:dst ~dep:src
        (fun env build -> Echo([Pathname.update_extension "o" (Pathname.basename (env src))], env dst));
      
      (* similar to ocamlbuild itarget -> otarget but uses the component extension and uses
         the same target name translation logic as other build rules here - supporting variants among other things *)
      let src, dst = "%.component", "%.stamp" in
      rule "component" ~stamp:dst ~dep:src
        (mk_component src dst);

      let src, dst = "%.component", "%.variant_%(variant).stamp" in
      rule "component variant" ~stamp:dst ~dep:src
        (mk_component src dst);
      
      (* lexer / statemachine compiler *)
      rule "Ragel: rl -> c" ~prod:"%.c" ~deps:["%.rl"; "%.rl.depends"]
        (mk_rl_c "%.rl" "%.c");
      flag ["rl"; "G1"] (S[A"-G1"]);
      flag ["rl"; "G2"] (S[A"-G2"]);
      flag ["rl"; "L"] (S[A"-L"]);

      (* useful for test results *)
      let src, dst = "%", "%.result" in
      rule "test result" ~prod:dst ~dep:src
        (fun env build -> Cmd (S [Sh (env src); C.redirect_out; Px (env dst)]))


    | After_rules ->      
    
      project();        
          
      (* Tool to scan a file for immediately included files
         custom scanners can be addeder earlier for specific extensions.
         The default looks for #include "file" and #include <file> *)
      rule "default immediate includes" ~prod:"%.includes" ~dep:"%"
        (mk_includes "%");      


      accumulated_include_rule "default accumulated includes" ~prod:"%.includes_acc" ~deps:["%"; "%.includes"];

      (* copy from includes_acc to .depends, use as example for more complex .depends rules
      
      rule "copy .includes_acc -> .depends" ~prod:"%.depends" ~deps:["%"; "%.includes_acc"]
        (mk_depends "%");
      *)        
      
      (* depends example with custom extension
    
      rule "copy .includes_acc -> .depends" ~prod:"%.c.depends" ~deps:["%.c"; "%.c.includes_acc"]
        (mk_depends "%");
      *)        
      
      (* this rule avoids building top-level includes_acc files and creates a .depends file directly *)
      accumulated_include_rule "default depends" ~prod:"%.depends" ~deps:["%"; "%.includes"];
            
      (* for convenience, always support the .exe extension *)
      naming_copy_rule "%.prog" "%.exe";

      (* shortcut for "ocamlbuild all" *)
      naming_copy_rule "all.stamp" "all";
    
      (* causes infinite loop if an invalid target is given
         as an alternative, the user can do a specific copy operation from a given .prog file
         to the desired target name, for example copy "hello.prog" "hello"
         or copy "hello.variant_release.prog" "fasthello" *)
      (* naming_copy_rule ~insert:`bottom "%.prog" "%"; *)

    (* end After_rules *)

    | _ -> ()
  
  end;; (* dispatch *)  
  
    (* set up generic tool flags *)
  let _ =
    (* C compiler and linker flags *)
    (* TODO: should also handle WIN64 targets and non-I386 targets on windows *)
    flag ["c";   "compile"; "msvc"]             (S[A"/nologo"; A"/W3"; A"/DWIN32"; A"/D_CONSOLE"]);
    flag ["c";   "compile"; "msvc"; "debug"]    (S[A"/Od"; A"/ZI"; A"/FD"; A"/RTC1"; A"/D_DEBUG"]);
    flag ["c";   "compile"; "msvc"; "release"]  (S[A"/O2"; A"/D_NDEBUG"]);
    
    flag ["c";   "compile"; "gnu";  "debug"]    (S[A"-g"]);
    flag ["c";   "compile"; "gnu";  "release"]  (S[A"-O2"]);
    
    flag ["c"; " link"; "msvc"]                 (S[A"/link"; A"/MACHINE:I386"; A"/SUBSYSTEM:CONSOLE"]);
        
    (* C++ compiler and linker flags *)
    flag ["cxx"; "compile"; "msvc"]             (S[A"/nologo"; A"/W3"; A"/DWIN32"; A"/D_CONSOLE"]);
    flag ["cxx"; "compile"; "msvc"; "std_exceptions"] (A"/EHsc"); (* /EHsc : enable exception handling *)
    flag ["cxx"; "compile"; "msvc"; "c_exceptions"]   (A"/EHs");  (* needed when exception is thrown via external C function *)
    flag ["cxx"; "compile"; "msvc"; "debug"]    (S[A"/Od"; A"/ZI"; A"/FD"; A"/RTC1"; A"/D_DEBUG"]);
    flag ["cxx"; "compile"; "msvc"; "release"]  (S[A"/O2"; A"/D_NDEBUG"; ]);
    
    flag ["cxx"; "compile"; "gnu";  "debug"]    (S[A"-g"]);
    flag ["cxx"; "compile"; "gnu";  "release"]  (S[A"-O2"]);

    flag ["cxx"; "link"; "msvc"]                (S[A"/link"; A"/MACHINE:I386"; A"/SUBSYSTEM:CONSOLE"]);
    
    (* C preprocessor dependency scanner flags
       - normally a heuristic scanner is used instead because it is faster and more practical *)
    flag ["c";   "depend"]                      (S[A"-M"; A"-Y"; A"-w0"; C.endian]);
    flag ["cxx"; "depend"]                      (S[A"-M"; A"-Y"; A"-D__cplusplus=199711L"]);;

    flag ["c_cxx"; "compile"; "gnu"; "warn_all"] (S[A"-Wall"]);
    flag ["c_cxx"; "compile"; "msvc"; "warn_all"] (S[A"/W4"; A"/Wp64"]);
    
end;; (* module Build_tools *)

module Build = struct

  let basedir () = (Sys.getcwd () / !Options.build_dir)

  let absolute path = MyPath.absolute path (basedir())
  
  (* enable exclusive tags *)
  let flag = Build_tools.flag ;;
  
  let copy = Build_tools.naming_map_copy_rule;;
  let copy_direct = Build_tools.naming_copy_rule;;
  let get_variant variant_tags path = Build_tools.get_variant variant_tags path;;

  (* make path absolute - otherwise it will be relative to individual files in different directories *)
  let c_include tags path =
    let path = absolute path in
    flag (["c_cxx"; "compile"] @ tags) (S[A"-I"; A path]);
    flag (["c_cxx"; "depend"] @ tags) (S[A"-I"; A path]);;

  let rl_include tags name path = flag (["rl"; "compile"] @ tags) (S[A"-I"; A path]);;

  (* TODO: probably need a gnu and msvc version of the library flag *)
  let c_library tags name path =
    let path = absolute path in
    flag (["c_cxx"; "link"; "program"] @ tags) (S[A("-L" ^ path); A("-l" ^ name)])

  let c_local_library tags name path =
    flag (["c_cxx"; "link"; "program"] @ tags) (A (path / name -.- C.ext_lib));;

  (*
      add macro definitions to C preprocessor
      examples:
        c_define ["TRACE"] "debug"
        c_define [] "VERSION=0.1.0"
        cxx_define [] "VERSION=0.1.0"
        
      If a C preprocessor dependency scanner is used, and if defines affect included files,
      then include files will be modified repeatedly for each variant since dep. scanning
      does not support per variant .depend files.
      It's not really worthwhile improving upon since a heuristic include pattern scanner is
      often the best choice - that is - assume every potentially included file
      must be available and up to date if it exists in the source tree.
  *)
  let c_define tags value =
    flag (["c_cxx"; "compile"; "msvc"] @ tags) (A("/D" ^ value));
    flag (["c_cxx"; "compile"; "gnu"]  @ tags) (A("-D" ^ value));
    flag (["c_cxx"; "depend"] @ tags) (A("-D" ^ value));;

  let dist_clib variant_tags name target_path include_path dist_path =
    let libname = (* "lib" ^*)  name in
    let use_tag = "use_" ^ libname in
    let debug = (dist_path / (libname ^ "d.a"))
    and release = (dist_path / (libname ^ ".a")) in
    c_include ["include_" ^ libname] include_path;
    c_local_library [use_tag;  "debug"] (name ^ "d") dist_path;
    c_local_library [use_tag; "~debug"] name         dist_path;
    copy (get_variant ("debug" :: variant_tags) target_path) debug;
    copy (get_variant ("release" :: variant_tags) target_path) release;
    dep [use_tag; "debug"; "link"] [C.map_target_ext debug];
    (* TODO: should be "~debug" instead of "release" or "default", but dep does not understand exclusive tags *)
    (* dep [use_tag; "~debug"; "link"] [release]; *)
    dep [use_tag; "release"; "link"] [C.map_target_ext release];
    dep [use_tag; "default"; "link"] [C.map_target_ext release]
  ;;

  let dist_ocaml_lib ocaml_lib_name src_path dist_path =
    List.iter (fun ext -> copy ((src_path / ocaml_lib_name) -.- ext) ((dist_path / ocaml_lib_name) -.- ext))
      ["a"; "mli"; "cmi"; "cma"; "cmxa"];
    ocaml_lib ~extern:true ~dir:dist_path (dist_path / ocaml_lib_name)
  ;;

  (* c_lib_name should be available in <dist_path> (or a rule to build it),
     <ocaml_lib_name>.mllib or .mlpack file must exist in <src_path>
     (or equivalent rule to produce .cm{x,}a archive.
     An <ocaml_lib_name>.ml file is not sufficient because c-library will be
     linked incorrectly as a dependency ("-cclib -lname" vs. "libname.a" in archive file).*)
  let dist_ocaml_clib ocaml_lib_name c_lib_name src_path dist_path =
    let clib = (dist_path / (c_lib_name ^ ".a")) in
    let link_clib = "link_clib_" ^ c_lib_name in

    flag ["link"; "byte"; "library"; "ocaml"; link_clib] (S [A "-custom"]);

    (* dep adds clib as an archive member when ocamlbuild creates .cm{x,}a from .ml file.
       so it is necessary to create a .mllib file *)
    dep ["link"; "library"; "ocaml"; link_clib] [clib];
    
    (* archive file should store -lmylib, not libmylib.a otherwise linking sometimes break *)
    let lib_flag = (S [ A "-cclib"; A ("-l" ^ (Util.prechomp "lib" c_lib_name)); A "-I"; A dist_path]) in
    flag ["link"; "library"; "ocaml"; link_clib] lib_flag;

    tag_file (src_path / (ocaml_lib_name ^ ".cmxa")) [link_clib];
    tag_file (src_path / (ocaml_lib_name ^ ".cma")) [link_clib];

    dep ["ocaml"; "native"; "use_" ^ ocaml_lib_name] [dist_path / (ocaml_lib_name ^ ".cmxa"); dist_path / ((ocaml_lib_name) ^ ".a")];
    dep ["ocaml"; "byte";   "use_" ^ ocaml_lib_name] [dist_path / (ocaml_lib_name ^ ".cma"); dist_path / ((ocaml_lib_name) ^ ".a")]; 

    dist_ocaml_lib ocaml_lib_name src_path dist_path
  ;;

    
  let dist_prog variant_tags name target_path dist_path =
    copy (get_variant ("debug" :: variant_tags) target_path)   (dist_path / (Build_tools.append_before_ext name "d"));
    copy (get_variant ("release" :: variant_tags) target_path) (dist_path / name);;

  let read_command = ReadProcess.read_process;;
  let read_command_line = ReadProcess.read_process_line;; 
  let read_command_lines = ReadProcess.read_process_lines;;

  let build = Build_tools.apply_project;;

end;;

