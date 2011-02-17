

let op = "<array>"
let cl = "</array>"
let entries = ref 0
let rand_value lev = incr entries; "foo" ^ (string_of_int lev) ^ "_" ^ (string_of_int !entries)

let min_rec = ref 3
let do_rec i = 
  let prob = 1. -. (0.1 *. float (i-!min_rec)) in
  Random.float 1. < prob
let do_adj i = 
  let i = max (i-!min_rec) 0 in
  Random.float 1. < 0.6 -. (0.1 *. float i)

let maxi = ref 1

let rec print_grp i =
  if i > !maxi then maxi := i;
  print_string op;
  if do_rec i then print_grp (i+1)
  else print_string (rand_value i);
  print_string cl;
  if do_adj i then print_grp i

let () = 
  gen_rec := Sys.argv.(1) |> int_of_string;
  Random.self_init ();
  print_endline "POST /InStock HTTP/1.1
Host: www.example.org
Content-Type: application/soap+xml; charset=utf-8

<?xml version=\"1.0\"?>
<soap:Envelope xmlns:soap=\"http://www.w3.org/2003/05/soap-envelope\">
  <soap:Header>
  </soap:Header>
  <soap:Body>";
  print_grp 1;
  print_endline "\n  </soap:Body>
</soap:Envelope>";
  Printf.eprintf "%d %d  " !maxi !entries;
