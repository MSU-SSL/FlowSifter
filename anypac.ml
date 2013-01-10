type t
type direction = Downflow | Upflow
external new_parser : unit -> t = "new_parser"
external add_data : t -> direction -> string -> unit = "add_data"
external get_event_count : unit -> int = "get_event_count"
(* external reset_parser : t -> unit = "reset_parser" *)
external delete_parser : t -> unit = "delete_parser"
