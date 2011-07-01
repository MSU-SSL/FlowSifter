open Batteries
open Future
open Gobject.Data
open Ns_types
open ParsedPCFG
open Printf

let in_files = ["http.pro"; "dns.pro"]
let filters = ["tcp port 80"; "udp port 53"]
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

let uniquify_ht = Hashtbl.create 17
let uniquify =
  fun s -> 
    try s ^ string_of_int (Hashtbl.find uniquify_ht s |> Ref.post_incr) 
    with Not_found -> Hashtbl.add uniquify_ht s (ref 1); s

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

let create_view ~packing (model : GTree.tree_store) =
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
  view#set_model (Some (model#coerce));
  view#selection#set_mode `NONE;
  view

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

let all_done (m: GTree.tree_store) g fn_pos () = 
  GMain.Main.quit ();
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
  let rec print_tree oc (N((n,_,p), rs)) =
    match p with
      (* don't have to print rules for terminals *)
      | Term n,_ -> (*Log.printf "T:%s\n%!" n*) ()
      (* print any rules for this non-terminal *)
      | Nonterm n_orig,_ -> 
	let prods = NTMap.find n_orig g.rules in
	let print_rule prod tree_nodes =	  
	  if not (VarMap.is_empty prod.predicates) then (
	    fprintf oc "%a:" (print_varmap print_pred) prod.predicates;	  
	  );
	  fprintf oc "X%s -> %t;\n" n 
	    (print_body prod.expression tree_nodes)
	in
	(*	Log.printf "NT:%s(%s)\nexprs:%a\ntree_nodes:%a\n%!" n n_orig
		(List.print print_production) prods 
		(print_tree_node |> List.print |> List.print) rs; *)
	if List.filter ((<>) []) rs <> [] then 
	  List.iter2 print_rule prods rs;
	List.iter (List.iter (print_tree oc)) rs
  in

  let oc,extr_file = File.open_temporary_out ~prefix:"extr" () in
  (match m#get_iter_first |> Option.get |> gen_tree with
    | [] -> failwith "No fields to extract, aborting"
    | [t] -> print_tree oc t
    | _ -> assert false (* can't have multiple toplevel items, only one start symbol is possible *)
  );
  IO.close_out oc;
  let proto_grammar = List.nth in_files fn_pos in
  let _filter = List.nth filters fn_pos in
  let new_parser = Prog_parse.new_parser proto_grammar extr_file in
  Demo.pcap_act "" new_parser

let create_combobox ~packing files =
  let model, column = GTree.store_of_list string files in
  let combo = GEdit.combo_box ~model ~packing () in
  combo#set_active 1;
  combo
  
let populate_model model g = 
  add_child model (Nonterm g.start,VarMap.empty);
  Option.may (add_children g model) model#get_iter_first

let main () = 
  (* init gtk *)
  GMain.Main.init () |> ignore;
  (* parse input grammar *)
  let g = ref (Ns_parse.parse_file_as_spec (List.hd in_files)) in
  (* the main window *)
  let window = GWindow.window ~allow_shrink:true ~allow_grow:true ~resizable:true ~width:window_width ~height:window_height () in
  let box1 = GPack.vbox ~packing:window#add ~spacing:3 () in
  let combo, (combo_store,combo_column) = 
    GEdit.combo_box_text ~packing:(box1#pack ~expand:false ~fill:false) ~strings:in_files () 
  in
  let get_fn () = combo_store#get ~row:(combo#active_iter |> Option.get) ~column:combo_column in 

  (* create and populate the model *)
  let model = GTree.tree_store cols in
  populate_model model !g;

  (* update the treeview based on filename *)
  combo#connect#changed (fun () -> 
    let fn = get_fn() in
    g := Ns_parse.parse_file_as_spec fn; 
    model#clear (); 
    Hashtbl.clear uniquify_ht;
    populate_model model !g;
  ) |> ignore;
  combo#set_active 0;

  (* create the treeview in the window *)
  let view = create_view ~packing:box1#add model in
  let populate i = if not (model#iter_has_child i) then add_children !g model i in
  let populate_children i = iter_children model i populate in
  view#connect#row_expanded ~callback:(fun i _p -> populate_children i) |> ignore;
  (* make sure we call all_done when the window is closed *)
  window#connect#destroy ~callback:(all_done model !g combo#active) |> ignore;
  (* show the window and start gtk event loop *)
  window#show ();
  GMain.Main.main ()

let () = main ()
