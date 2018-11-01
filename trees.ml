(* A binary search tree for implementing key/value maps *)
type 'a tree =
  | Leaf
  | Node of int * 'a * 'a tree * 'a tree

let t1 = Node (4, 3, Node (2, 4, Leaf, Leaf), Leaf)

let t2 = Node (4, 3, Node (2, 4, Leaf, Leaf), Leaf)

let rec update (t: 'a tree) k v : 'a tree =
  match t with
  | Leaf -> Leaf
  | Node (k1, v1, l, r) ->
    if k = k1 then Node (k1, v, l, r)
    else
    if k < k1 then Node (k1, v1, update l k v, r)
    else Node (k1, v1, l, update r k v)

let is_sorted t =
  (* let rec check min max t = match t with *)
  (* let rec check min max = fun t -> match t with *)
  let rec check min max = function
  | Leaf -> true
  | Node (k, _, l, r) ->
    min <= k && k <= max && 
    check min (k - 1) l &&
    check (k + 1) max r
  in
  check min_int max_int t

(*type 'a option =
  | None
  | Some of 'a*)

let rec find (t: 'a tree) k : 'a option = match t with
  | Leaf -> None
  | Node (k1, v, l, r) ->
    if k1 = k then Some v
    else if k < k1 then find l k else find r k

let _ = match find t2 2 with
| Some v -> print_endline (string_of_int v)
| None -> print_endline "key not found!"

let rec remove_duplicates = function
  | [] -> []
  | x :: (y :: _ as ys) when x = y ->
    remove_duplicates ys
  | x :: xs -> x :: remove_duplicates xs



