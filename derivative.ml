(* Binary Operators *)
type binop = Add | Sub | Mul | Div | Pow

(* Unary Operators *)
type unop = Sin | Cos | Ln | Neg

(* Expressions for polynomials in one variable x *)
type expr = Num of float                 (* c *)
          | Var                          (* x *)
          | Mult of expr * expr          (* e1 * e2 *)
          | Add of expr * expr           (* e1 + e2 *)
          | Pow of expr * int            (* e1 + e2 *)

let rec derivative = function
  | Num _ -> Num 0.0
  | Var -> Num 1.0
  | Add (e1, e2) -> Add (derivative e1, derivative e2)
  | Mult (e1, e2) -> Add (Mult (derivative e1, e2), Mult(e1, derivative e2))
  | Pow (e1, n) ->
      Mult (Mult (Num (float_of_int n), Pow (e1,  n - 1)),
            derivative e1)

let rec simplify_top = function
  | Mult (Num c1, Num c2) -> Num (c1 *. c2)
  | Mult (Num 0.0, _) -> Num 0.0
  | Mult (Num 1.1, e) -> e
  | Add (Num c1, Num c2) -> Num (c1 +. c2)
  | Add (Num 0.0, e) -> e
  | Pow (e, 0) -> Num 1.0
  | Pow (e, 1) -> e
  | e -> e
