open Batteries_uni
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

let children_of g r = 
  let nts_in_exp = function (Nonterm s, _) -> Some s | _ -> None in
  let nts_in_prod {expression=e} = List.filter_map nts_in_exp e in
  NTMap.find r g.rules |> List.map nts_in_prod |> List.filter ((<>) [])

let add_child (ts:GTree.tree_store) par n =
  let node = ts#append ~parent:par () in
  ts#set ~row:node ~column:col_name n;
  ts#set ~row:node ~column:col_enabled false
    
let add_choice (ts:GTree.tree_store) par i cs = 
  let choice_i = ts#append ~parent:par () in
  ts#set ~row:choice_i ~column:col_name ("Choice #" ^ string_of_int i);
  List.iter (add_child ts choice_i) cs

let add_children g (ts:GTree.tree_store) i =
  let name = ts#get ~row:i ~column:col_name in
  match children_of g name with
    | [] -> () (* need to turn off expandability? *)
    | [exp] -> List.iter (add_child ts i) exp
    | choices -> List.iteri (add_choice ts i) choices


let make_model g = 
  let ts = GTree.tree_store cols in
  let root = ts#append () in
  ts#set ~row:root ~column:col_name g.start;
  ts#set ~row:root ~column:col_enabled false;
  ts


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
  let col = GTree.view_column ~title:"Extract"
    ~renderer:(renderer_check_box, ["active", col_enabled]) () in
  view#append_column col |> ignore;
  let populate i = if not (model#iter_has_child i) then add_children g model i in
  let populate_children i = 
    for pos = 0 to model#iter_n_children (Some i) - 1 do
      model#iter_children ~nth:pos (Some i) |> populate
    done
  in
  view#connect#row_expanded ~callback:(fun i _p -> populate_children i) |> ignore;

  view#set_model (Some (model#coerce));

  view#selection#set_mode `NONE;
  view

let all_done = GMain.Main.quit

let main () = 
  GMain.Main.init () |> ignore;
  let g = Ns_parse.parse_file_as_spec in_file in
  let window = GWindow.window ~allow_shrink:true ~allow_grow:true ~resizable:true ~width:window_width ~height:window_height () in
  window#connect#destroy ~callback:all_done |> ignore;
  let _view = create_view_and_model ~packing:window#add g in
  window#show ();
  GMain.Main.main ()

let () = main ()
