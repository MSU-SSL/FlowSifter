

let op = "<array>"
let cl = "</array>"
let entries = ref 0
let rand_value lev = incr entries; "foo" ^ (string_of_int lev) ^ "_" ^ (string_of_int !entries)

let do_rec min_rec i = 
  let prob = 1. -. (0.1 *. float (i - min_rec)) in
  Random.float 1. < prob
let do_adj min_rec i = 
  let i = max (i - min_rec) 0 in
  Random.float 1. < 0.6 -. (0.1 *. float i)

let maxi = ref 1

let rec output_grp min_rec print i =
  if i > !maxi then maxi := i;
  print op;
  if do_rec min_rec i then output_grp min_rec print (i+1)
  else print (rand_value i);
  print cl;
  if do_adj min_rec i then output_grp min_rec print i

let output_soap min_rec print = 
  print "POST /InStock HTTP/1.1
Host: www.example.org
Content-Type: application/soap+xml; charset=utf-8

<?xml version=\"1.0\"?>
<soap:Envelope xmlns:soap=\"http://www.w3.org/2003/05/soap-envelope\">
  <soap:Header>
  </soap:Header>
  <soap:Body>";
  output_grp min_rec print 1;
  print "\n  </soap:Body>
</soap:Envelope>";
  ()


let () = 
  Random.self_init ();
  let min_rec = int_of_string Sys.argv.(1) in
  let count = int_of_string Sys.argv.(2) in
  let path = Sys.argv.(3) in
  for i = 1 to count do
    let fn = path ^ (string_of_int i) in
    let oc = open_out fn in
    output_soap min_rec (output_string oc);
    close_out oc;
  done;
  ()
