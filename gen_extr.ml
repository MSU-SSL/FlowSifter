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

let make_model g = 
  let ts = GTree.tree_store cols in
  let root = ts#append () in
  ts#set ~row:root ~column:col_name g.start;
  ts#set ~row:root ~column:col_enabled false;
  ts


let create_view_and_model ~packing g =
  let model = make_model g in

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

  view#set_model (Some (model#coerce));

  view#selection#set_mode `NONE;
  view

let all_done = GMain.Main.quit

let main () = 
  GMain.Main.init ();
  let g = Ns_parse.parse_file_as_spec in_file in
  let window = GWindow.window ~allow_shrink:true ~allow_grow:true ~resizable:true ~width:window_width ~height:window_height () in
  window#connect#destroy ~callback:all_done |> ignore;
  let _view = create_view_and_model ~packing:window#add g in
  window#show ();
  GMain.Main.main ()

let () = main ()
