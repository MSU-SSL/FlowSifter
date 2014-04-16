(** Disjoint set library *)

type 'a node = {mutable parent: 'a node; mutable rank: int; label: 'a}

let singleton x = 
  let rec n = {parent=n; rank=0; label=x} in
  n

let rec find x = 
  if x.parent == x then
    x
  else (
    x.parent <- find x.parent;
    x.parent
  )

let union x y =
  let xr = find x
  and yr = find y in
  if xr.rank > yr.rank then
    yr.parent <- xr
  else if yr.rank > xr.rank then
    xr.parent <- yr
  else if xr != yr then begin
    yr.parent <- xr;
    xr.rank <- xr.rank + 1
  end
    

let find_label x = (find x).label

module IntDSet = struct
  let make n = Array.init n (fun i -> singleton i)
  let find ds i = find_label ds.(i)
  let union ds i j = union ds.(i) ds.(j)
end

module Lab_IntDSet = struct
  let make n label_f = Array.init n (fun i -> singleton (i,label_f i))
  let findl ds i = find_label ds.(i)
  let union ds i j = union ds.(i) ds.(j)
end
