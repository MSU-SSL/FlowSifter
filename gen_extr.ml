open Batteries_uni
open Future
open Gobject.Data
open Ns_types
open ParsedPCFG
open Printf

let in_file = "spec.ca"
(* TODO: out_file *)
let window_width = 500 and window_height = 700

let cols = new GTree.column_list
let col_name = cols#add string
let col_enabled = cols#add boolean
let col_choice = cols#add boolean
let col_prod = cols#add caml_option

let children_of_nonterm g r = 
  let nts_in_exp = function (Nonterm s, _) -> Some s | _ -> None in
  let _nts_in_prod {expression=e} = List.filter_map nts_in_exp e in
  NTMap.find r g.rules (*|> List.map nts_in_prod*) (*|> List.filter ((<>) [])*)

let uniquify =
  let ht = Hashtbl.create 17 in
  fun s -> try s ^ string_of_int (Hashtbl.find ht s |> Ref.post_incr) with Not_found -> Hashtbl.add ht s (ref 1); s

let add_child (ts:GTree.tree_store) ?parent prod =
  let row = ts#append ?parent () in
  ts#set ~row ~column:col_enabled false;
  ts#set ~row ~column:col_prod (Some prod);
  match prod with 
    | (Term rx,_) ->
      ts#set ~row ~column:col_name rx
    | (Nonterm n,_) -> 
      ts#set ~row ~column:col_name (uniquify n)
    
let add_choice (ts:GTree.tree_store) par i p = 
  ts#set ~row:par ~column:col_choice true;
  let choice_i = ts#append ~parent:par () in
  ts#set ~row:choice_i ~column:col_prod None;
  ts#set ~row:choice_i ~column:col_name ("Choice #" ^ string_of_int i);
  List.iter (add_child ts ~parent:choice_i) p.expression

(* Add one level of children *)
let add_children g (ts:GTree.tree_store) i =
  match ts#get ~row:i ~column:col_prod with 
    | None -> ()
    | Some (Term rx,_) -> () (* can't expand a terminal *)
    | Some (Nonterm name, _) ->
	match NTMap.find name g.rules with
	  | [] -> () 
	  | [prod] -> List.iter (add_child ts ~parent:i) prod.expression
	  | choices -> List.iteri (add_choice ts i) choices

let make_model g = 
  let ts = GTree.tree_store cols in
  add_child ts (Nonterm g.start,VarMap.empty);
  ts

let iter_children (m: GTree.tree_store) i f = 
  for pos = 0 to m#iter_n_children (Some i) - 1 do
    f (m#iter_children ~nth:pos (Some i))
  done

let children_of_iter m i = 
  let acc = ref [] in iter_children m i (fun c -> acc := c :: !acc); List.rev !acc

let iter_tree (m: GTree.tree_store) f =
  let root = m#get_iter_first |> Option.get in
  let rec act i = f i; iter_children m i act in
  iter_children m root act

let create_view_and_model ~packing g =
  let model = make_model g in
  Option.may (add_children g model) model#get_iter_first;

  let view = GTree.view ~packing () in

  let col = GTree.view_column ~title:"Field Name"
    ~renderer:(GTree.cell_renderer_text [], ["text", col_name]) () in
  view#append_column col |> ignore;

  let renderer_check_box = GTree.cell_renderer_toggle [`ACTIVATABLE true] in
  let toggle_box gtk_tree_path = 
    let modelIter = model#get_iter gtk_tree_path in
    let existing_val = model#get ~row:modelIter ~column:col_enabled in
    model#set ~row:modelIter ~column:col_enabled (not existing_val);
  in
  renderer_check_box#connect#toggled ~callback:toggle_box |> ignore;
  view#connect#row_activated ~callback:(fun p _ -> toggle_box p) |> ignore;
  let col = GTree.view_column ~title:"Extract"
    ~renderer:(renderer_check_box, ["active", col_enabled]) () in
  view#append_column col |> ignore;
  let populate i = if not (model#iter_has_child i) then add_children g model i in
  let populate_children i = iter_children model i populate in
  view#connect#row_expanded ~callback:(fun i _p -> populate_children i) |> ignore;

  view#set_model (Some (model#coerce));

  view#selection#set_mode `NONE;
  model, view

type 'a gram_tree = N of 'a * 'a gram_tree list list

let print_tree_node oc (N((n,e,_),_)) = 
  if e then 
    fprintf oc "bounds( %s )" n
  else
    IO.nwrite oc n

let print_rule oc prods = 
  List.print ~first:"" ~last:"" ~sep:" " print_tree_node oc prods

let rec print_body prod_list tree_nodes oc = match prod_list, tree_nodes with
  | [], [] -> ()
  | [], _::_ -> assert false
(*    Log.printf "Empty prod list, tree nodes: \n%a\n" (List.print print_tree_node) tree_nodes *)
  | (Nonterm n_orig,acts as ph)::pt, (N((n,e,p),ns))::tt when p == ph -> 
    let n = if List.filter ((<>) []) ns = [] then n_orig else "X" ^ n in
    if e then fprintf oc "bounds( %s )" n
    else IO.nwrite oc n;
    print_varmap print_action oc acts;
    IO.write oc ' ';
    print_body pt tt oc
  | (Term n,acts)::pt, t
  | (Nonterm n, acts)::pt, t ->
    IO.nwrite oc n; 
    print_varmap print_action oc acts;
    IO.write oc ' ';
    print_body pt t oc

let all_done (m: GTree.tree_store) g () = 
  let rec gen_tree i =
    let p = m#get ~row:i ~column:col_prod |> Option.get in
    match p with
      | (Term _, _) -> []
      | (Nonterm _,_) -> 
	let ch = m#get ~row:i ~column:col_choice in
	let children = children_of_iter m i in
	let rules = if ch then List.map (children_of_iter m) children else [children] in
	let child_nodes = List.map (List.map gen_tree |- List.flatten) rules (*|> List.filter ((<>) [])*) in
	let n = m#get ~row:i ~column:col_name in 
	let e = m#get ~row:i ~column:col_enabled in
	if List.filter ((<>) []) child_nodes = [] && not e then [] else
	  [N ((n,e,p),child_nodes)]
  in
  let rec print_tree (N((n,_,p), rs)) =
    match p with
      (* don't have to print rules for terminals *)
      | Term n,_ -> (*Log.printf "T:%s\n%!" n*) ()
      (* print any rules for this non-terminal *)
      | Nonterm n_orig,_ -> 
	let prods = NTMap.find n_orig g.rules in
	let print_rule prod tree_nodes =
	  
	  printf "%a:X%s -> %t\n" 
	    (print_varmap print_pred) prod.predicates 
	    n 
	    (print_body prod.expression tree_nodes)
	in
	Log.printf "NT:%s(%s)\nexprs:%a\ntree_nodes:%a\n%!" n n_orig
	  (List.print print_production) prods 
	  (print_tree_node |> List.print |> List.print) rs;
	if List.filter ((<>) []) rs <> [] then 
	  List.iter2 print_rule prods rs;
	List.iter (List.iter print_tree) rs
  in

  let tree_list = m#get_iter_first |> Option.get |> gen_tree in
(*  let new_root = (N(("EXTR", false, (Nonterm "EXTR", VarMap.empty)), [t])) in *)
  print_tree (List.hd tree_list);
  GMain.Main.quit ()

let main () = 
  (* init gtk *)
  GMain.Main.init () |> ignore;
  (* parse input grammar *)
  let g = Ns_parse.parse_file_as_spec in_file in
  (* the main window *)
  let window = GWindow.window ~allow_shrink:true ~allow_grow:true ~resizable:true ~width:window_width ~height:window_height () in
  (* create the treeview in the window *)
  let model, _view = create_view_and_model ~packing:window#add g in
  (* make sure we call all_done when the window is closed *)
  window#connect#destroy ~callback:(all_done model g) |> ignore;
  (* show the window and start gtk event loop *)
  window#show ();
  GMain.Main.main ()

let () = main ()
