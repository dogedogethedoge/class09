# Class 9

## OCaml

### Hindley-Milner Type System

We start by discussing OCaml type system in more depth. The type
system is derived from the so-called Hindley-Milner type system (also
Damas-Hindley-Milner) or HM for short. HM is the foundation of the
type systems used by all languages in the ML-family.

#### Type Compatibility

Type compatibility in HM is based on the notion
of
[*unification*](https://en.wikipedia.org/wiki/Unification_(computer_science)).

A *substitution* σ for a type `t` is a mapping of the type variables
occurring in `t` to types. Example: a possible substitution for
the type `'a -> 'b` is the mapping σ = `{'a ~> int, 'b ~> int}`. Another
one is σ'=`{'a ~> 'a, 'b ~> 'b}`. 

Applying a substitution σ to a type `t`, written `t`σ, results in the
type `t` where all occurrences of type variables have been replaced
with according to σ. For instance, if `t` = `'a -> 'b` and σ = `{'a
-> int, 'b -> int}`, then `t`σ = `int -> int`.

Two types `s` and `t` are said to be *unifiable* if there exists a
substitution σ for the type variables occurring in both `s` and `t`
such that `s`σ = `t`σ. The substitution σ is called a *unifier* for `s`
and `t`. Example: the (unique) unifier for `s = 'a -> int` and `t =
string -> 'b` is σ = `{'a ~> string, 'b ~> int}`. On the other hand,
there exists no unifier for `t = 'a -> int` and `s = string -> 'a`
because `string` and `int` are distinct types.

A type `t` is *more general* than a type `s` if there exists a unifier σ
for `t` and `s` such that `s`σ = `s`. Example: the type `'a -> 'b` is
more general than the type `int -> 'b`.

A value `v` of type `t` is compatible with type `s` (i.e. `v` can be
used in a context that expects a value of type `t`) if `t` is more
general than `s`. This definition may seem counter-intuitive at
first. However, it captures the intuition that the more general type `t`
denotes a smaller set of values than `s` and, hence, every value of
type `t` is also a value of type `s`. To see this, consider the types
`t = 'a -> int` and `s = int -> int`, then `t` is more general than
`s`. Now take the expression `f 3 + 1`. Here, the context of `f`
expects `f` to be a function of type `int -> int` because we are
calling `f` with value `3` which is of type `int` and we are using the
result of the call in an addition operation using `+` which also
expects arguments of type `int`.

Now, if the actual type of `f` is `'a -> int`, then this tells us that
we can call `f` with any value we like, including `int` values and the
result is always going to be an `int`. Thus it is safe to use `f` in
this context.

### Type Inference

One remarkable feature of the type system of the languages in the ML
family is the ability to algorithmically infer the type `t` of any
expression `e` and at the same time check whether `e` is
well-typed. This yields all the benefits of a strong static type
system without requiring the programmer to provide explicit type
annotations in the program.

The algorithm is guided by the syntax of expressions, taking advantage
of the fact that the constant literals, inbuilt operators, and value
constructors from which expressions are built impose constraints on
the types that the values of these expressions may have. Technically,
the algorithm works by deriving a system of type equality constraints
from every expression and then solving this constraint system by
computing a *most general unifier* that satisfies all the type
equalities simultaneously.

At a high-level, the algorithm can be summarized as follows. Given a
program expression `e` whose type is to be inferred, perform the
following steps:

1. Associate a fresh type variable with each subexpression occurring
   in `e`. These type variables should be thought of as placeholders
   for the actual types that we are going to infer.
   
2. Associate a fresh type variable each each program variable
   occurring in `e`. These type variables are placeholders for the
   types of those program variables.

3. Generate a set of type equality constraints from the syntactic
   structure of `e`. These typing constraints relate the introduced
   type variables with each other and impose restrictions on the types
   that they stand for.

4. Solve the generated typing constraints. If a solution of the
   constraints exists (a unifier), the expression is well-typed and we
   can read off the types of all subexpressions (including `e` itself)
   from the computed solution. Otherwise, if no solution exists, `e` has
   a type error.

Rather than describing this algorithm formally, we explain it through
a series of examples. A complete description of the algorithm can be
found
[here](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system) as
well
as
[Robin Milner's original article](https://www.sciencedirect.com/science/article/pii/0022000078900144/pdf?md5=cdcf7cdb7cfd2e1e4237f4f779ca0df7&pid=1-s2.0-0022000078900144-main.pdf). A
good textbook-treatment can be found in Ch. 22
of
[Types and Programming Languages.](https://www.cis.upenn.edu/~bcpierce/tapl/) by
Benjamin Pierce.

As a first example, let us consider the following expression `e`:

```ocaml
x + f 1
```

Step 1: associate a type variable with every subexpression of `e`:

```
x : 'a
f : 'b
1 : 'c
f 1 : 'd
x + f 1 : 'e
```

Step 2: associate a type variable with every variable occurring in `e`:

`x: 'x`
`f: 'f`

Step 3: generate typing constraints by analyzing the syntactic
structure of the expression `e`.

From the top-level operator `+` in the expression `x + f 1` we can
infer that the types of the subexpressions `x` and `f 1` must both be
`int` because `+` assumes integer arguments in OCaml (floating point
addition is given by the operator `+.`). Moreover, we conclude that
the whole expression `e` must have type `int`, since `+` again
produces an integer value. Hence, we must have:

```
'a = int
'd = int
'e = int
```

Similarly, from the subexpression `f 1` whose top-level operation is a
function application, we can infer that the type of `f` must be a
function type whose argument type is the type of `1` and whose result
type is the type of `f 1`. That is, we must have:

`'b = 'c -> 'd`

Finally, we look at the leaf expressions `x`, `f`, and `1`. For `x`
and `f` we obtain the constraints relating the type variables
associated with those leaf expressions with the type variables
associated with the corresponding program variables:

```
'x = 'a
'f = 'd
```

The leaf expression `1` is an integer constant whose type
is `int`. Thus we have:

`'c = int`

Thus altogether, we have gathered the following constraints:

```
'a = int
'd = int
'e = int
'b = 'c -> 'd
'c = int
'x = 'a
'f = 'd
```
Step 4: solve the typing constraints.

Solving the constraints amounts to computing a unifier σ (a mapping
from type variables to types) such that when σ is applied to both
sides of each typing constraint makes the two sides syntactically
equal. If such a unifier does not exist, then this means that at least
one of the typing constraints cannot be satisfied, and there must be a
type error in the expression `e`. Otherwise, once we have computed the
unifier σ, we can read off the types of all the subexpressions of `e`
directly from σ.

The problem of finding a unifier for a set of typing constraints is
called a *unification problem*. We can compute a unifier σ from the
set of constraints using a simple iterative algorithm. The algorithm
starts with a trivial candidate solution σ₀ = ∅ and then processes the
equality constraints one at a time in any order, extending σ₀ to an
actual unifier of the whole set of constraints. If at any point, we
encounter a constraint that we cannot solve (e.g. `bool = int`), then
we aboard and report a type error.

For our example, we process the constraints in the order given above,
starting with the constraint

`'a = int`

For each constraint, we first apply our current candidate unifier,
here σ₀, to both sides of the equality and then solve the resulting
new equality constraint. In our first iteration of this algorithm, σ₀
is the empty substitution, so applying σ₀ to the constraint does not
change the constraint. Thus, we solve

`'a = int`

Solving a single type equality constraint `t1 = t2` for arbitrary type
expressions `t1` and `t2` is done like this:

Case 1. If `t1` and `t2` are the exact same type expressions, then there is
  nothing to be done. We simply proceed to the next equality
  constraint using our current candidate unifier.

Case 2. If one of the two types `t1` and `t2` is a type variable, say
  `t1 = 'a`, then we first check whether `'a` occurs in `t2`. If it
  does, then there is no solution to the typing constraints and we
  aboard with a type error. The is referred to as the "*occurs check*"
  (more on this case later). Otherwise, we extend the current
  candidate unifier with the mapping `'a ~> t2` and additionally
  replace any occurrence of `'a` on the right side of a mapping in the
  current candidate unifier with `t2`.
  
Case 3. If `t1` or `t2` are both types constructed from the same type
  constructor, e.g., suppose `t1` is of the form `t11 -> t12` and `t2`
  is of the form `t21 -> t22`, then we simply replace the constraint
  `t1 = t2` with new constraints equating the corresponding component
  types on both sides: `t11 = t21` and `t12 = t22`. Then we proceed
  with the new set of constraints and our current candidate unifier.
  
Case 4. In all other cases, we must have a type mismatch. That is, `t1` and
  `t2` must be both types constructed from some type constructor and
  those type constructors must be different. Hence, the two types
  cannot be made equal no matter what unifier we apply to them. Note
  that we can view primitive types like `int` and `bool` as type
  constructors that take no arguments. So a type mismatch like `int =
  bool` is also covered by this case.
  
In our example, the second case applies. Hence we extend σ₀ to 

σ₁ = { `'a ~> int` }

and proceed to the next constraint. The next two constraints

```
'd = int
'e = int
```

are similar to the first one and we just update the candidate unifier
to:

σ₃ = { `'a ~> int`, `'d ~> int`, `'e ~> int` }

The next constraint is

`'b = 'c -> 'd`

we apply σ₃ on both sides and obtain

`'b = 'c -> int`

and then update our candidate solution to

σ₄ = { `'a ~> int`, `'d ~> int`, `'e ~> int`, `'b ~> 'c -> int` }

After processing the remaining three constraints 

```
'c = int
'x = 'a
'f = 'b
```

we obtain the following solution

σ = { `'a ~> int`, `'d ~> int`, `'e ~> int`, `'b ~> int -> int`, 
      `'c ~> int`, `'x ~> int`, `'f ~> int -> int` }

Note that if we apply σ to all the original constraints derived from
`e`, then we observe that σ is indeed a correct solution of the constraint system:

```
int = int
int = int
int = int
int -> int = int -> int
int = int
int = int
int = int
```

Moreover, we can now read off the inferred types for all
subexpressions in `e` directly from σ. For instance, we have σ(`'e`) =
`int`, indicating that the type of expression `e` itself is
`int`. Similarly, the inferred types for the variables `x` in `e` is
σ(`'x`)=`int`, and the type of variable `f` is σ(`'f`)=`int ->
int`. Thus, we had e.g. a larger expression that defines a function
`g` in terms of the expression `e` like this:

```ocaml
let g x f = x + f 1
```

then we can immediately conclude that the type of `g` must be

`g: int -> (int -> int) -> int`

#### Detecting Type Errors

In the previous example, the type inference succeeded and we were able
to show that the expression is well-typed. Let's look at an example
where this is not the case. Consider the expression

`if x then x + 1 else y`

We proceed as in the previous example and first introduce type variables for all subexpressions:

```
x: 'a
x: 'b
1: 'c
x + 1: 'd
y: 'e
if x then x + 1 else y: 'f
```

Note that we introduce two type variables for `x` one for each
occurrence of `x` in the expression.

Next, we associate a type variable with each program variable
appearing in the expression:

```
x: 'x
y: 'y
```

Then we generate the typing constraints from all the
subexpressions. From the first occurrence of `x` we infer

`'a = 'x`


From `x + 1` and its subexpressions we infer:

```
'b = 'x
'b = int
'c = int
'd = int
```

From `y` we infer

```
'e = 'y
```

and from `if x then x + 1 else y` we infer

```
'a = bool
'f = 'd
'f = 'e
```

The first of the last three constraints enforces that the condition
being checked, here `x`, must have type `bool`.  The other two
constraints enforce that the two branches of the conditional have the
same type, which is also the type of the entire conditional expression
(since the result value of the conditional is determined by the result
value of the branches).

Thus, in summary, we have the constraints:

```
'a = 'x
'b = 'x
'b = int
'c = int
'd = int
'a = bool
'f = 'd
'f = 'e
```

Now we solve the constraints. Processing the first 5 constraints is
fine and produces the candidate solution

σ₅ = { `'a ~> int`, `'b ~> int`, `'x ~> int`, `'c ~> int`, `'d ~> int` }

However, when we process the next constraint

`'a = bool`

and apply σ₅ to both sides, we get:

`int = bool`

which is unsolvable. Thus at this point we aboard and report a type error.

If we look back at the original expression

`if x then x + 1 else y`

then we note that this expression uses `x` both in a context that
expects it to be an `int` value and in an context that expects it to
be of type `bool`. Since it can't be both at the same time, we have a
type error. This is exactly what the type inference deduced for us.

#### Inferring Polymorphic Types

One of the great features of ML-style type inference is that solving
the unification problem may produce a unifier that leaves some of the
type variables unconstrained. Such solutions yield polymorphic types. 

As an example consider again our polymorphic function `flip` that
takes a curried function `f` and produces a new function that behaves like `f` but takes its arguments in reverse order:

```ocaml
let flip f x y = f y x
```

To infer the types we proceed as above and solve the type inference problem for the body expression

`f y x`

of the function.

Start with assigning type variables to all subexpressions:

```
f: 'a
y: 'b
x: 'c
f y: 'd
f x y: 'e
```

then assign type variables to all program variables:

```
f: 'f
x: 'x
y: 'y
```

Generate the constraints: from `f`, `y`, and `f` we infer

```
'a = 'f
'b = 'y
'c = 'x
```

From `f y` we infer

`'a = 'b -> 'd`

and from `f y x` we infer

`'d = 'c -> 'e`

Thus, in summary we have:

```
'a = 'f
'b = 'y
'c = 'x
'a = 'b -> 'd
'd = 'c -> 'e
```

Solving the constraints yields the unifier:

σ = { `'a ~> 'y -> 'x -> 'e`, `b ~> 'y`, `c ~> 'x`, 
      `'f ~> 'y -> 'x -> 'e`, `d ~> 'x -> 'e` }

Now from the definition of `flip` we know that the type of `flip` is

`flip: 'f -> 'x -> 'y -> 'e`

After applying σ to this type we get:

`flip: ('y -> 'x -> 'e) -> 'x -> 'y -> 'e`

This type tells us that `flip` takes a function of type `'y -> 'x ->
'e` and returns a new function of type `'x -> 'y -> 'e`, which
captures that the arguments in the returned function are reversed. The
type is parametric in the type variables `'x`, `'y`, and `'e`. That
is, we can safely apply `flip` to any function that is compatible with
the type `'y -> 'x -> 'e`, including e.g.

```
int -> int -> int
int -> bool -> bool
bool -> int -> int
(int -> bool) -> int -> int
...
```

Also, note that since the specific names of the type variables
appearing in the inferred types do not matter, we can consistently
rename them once the type inference is complete. For instance, if we
rename `'y ~> 'a`, `x ~> 'b`, and `'e ~> 'c`, then we get the type:

`flip: ('a -> 'b -> 'c) -> 'b -> 'a -> 'c`

This is exactly the type that the OCaml compiler infers for `flip`.


#### Limitations of Type System

HM guarantees at compile-time that a program will not *go wrong* when
it is executed if it passes the type checker. Here, "going wrong"
means that the program may get stuck, trying to use a value in a
context it was not supposed to be used (e.g. try to treat an integer
value as if it were a function that can be called). We thus get very
strong correctness guarantees from the compiler. However, as with any
static type system, these guarantees come at a certain cost, namely
that the type checker will reject some programs even though they will
not fail when they are executed. That is, the type checker will reject
all bad programs but also some good ones.

One crucial restriction of the type system has to do with
polymorphism. Only variables that are introduced by `let` bindings can
have polymorphic types. For instance the following Ocaml expression
cannot be typed:

```ocaml
let g f = (f true, f 0) in
let id x = x in
g id
```

The problem here is that the function `f` in `g` is not introduced by
a `let` and therefore can't have a polymorphic type itself. Only `g`'s
type can be polymorphic. That is, once `g` has been applied to a
particular function `f`, the type of that `f` is treated
monomorphically. In contrast, the following expression is OK:

```ocaml 
let f x = x in 
(f true, f 0) 
``` 

Note that we here provide a specific polymorphic implementation of the
function `f` which we bind with a `let`, rather than parameterizing
over the implementation of `f` as in `g` above.

Due to the nature of this restriction one also refers to the form of
polymorphism supported by HM as *let-polymorphism*. Let-polymorphism
is less expressive than the kind of polymorphism supported by other
static type systems, specifically generics in languages like Java, C#,
and Scala. This problem also does not arise in dynamically typed
languages like Scheme. For instance, the corresponding Scheme program
will execute just fine:

```scheme
(define (g f) (cons (f #t) (f 0)))
(define (id x) x)
(g id)
```

The restriction imposed by let-polymorphism is critical for making
static type inference possible. In particular, Java, C#, and Scala do
not support general static type inference because their type systems
are more expressive. This is an example of a trade-off between static
correctness guarantees, automation, and expressivity in the design of
a type system where different languages make different design choices.

Another limitation of HM has to do with how the type inference
algorithm infers the types of recursive functions. Specifically, this
relates to the "occurs check" during the computation of the unifier in
Case 2 of the algorithm to solve type equations.

To illustrate the issue, consider the following expression

```ocaml
let rec print_more x =
  let _ = print_endline x in
  print_more
```

This function can be viewed as an output channel that we can simply
feed more and more values which are then just printed on standard output:

```ocaml
print_more "Hello" "World" "how" "are" "you"
```

While the function `print_more` should behave just fine when it
executes (and a corresponding Scheme version indeed works as
expected), it is rejected by the OCaml type checker:

```ocaml
# let rec print_more x =
    let _ = print_endline x in
    print_more;;
line 3, characters 4-14:
Error: This expression has type string -> 'a
       but an expression was expected of type 'a
       The type variable 'a occurs inside string -> 'a
```

To understand how this problem arises, consider the following slightly
simplified version of `print_more`:

```ocaml
let rec hungry x = hungry
```

If we apply our type inference algorithm, we generate the following
type variables for the parameters of the recursive definition:

```
hungry: 'h
x: 'x
```

and the following typing constraints from the actual definition:

```
'h = 'x -> 'a
'a = 'h
```

After solving the first constraint we obtain the candidate solution

σ₁ = { `'h ~> 'x -> 'a` }

which when applied to the second constraint yields

`'a = 'x -> 'a`

At this point we cannot extend σ₁ with a mapping for `'a` because the
occurs check fails: `'a` occurs in the type `'x -> 'a` on the
right-hand side of the equation. 

The equation

`'a = 'x -> 'a`

tells us that the return type of `hungry` is the same type as the type
of `hungry` itself. However, such recursive types cannot be expressed
in OCaml's type system directly for otherwise, the type inference
problem would again become unsolvable. As we shall see, there is a
workaround for this restriction: OCaml allows recursive types to be
introduced explicitly. However, this workaround slightly complicates
the implementation of the function `hungry`.


### Algebraic Datatypes and Pattern Matching

An important and very convenient feature supported by many functional
programming languages is the notion of *algebraic datatypes* (ADTs).
ADTs provide type constructors for building user-defined immutable
tree-like data structures. They are known as *variant types* and
*(disjoint) sum types*. Together with *pattern matching*, ADTs enable
very powerful programming techniques.

Here is an example of an ADT in OCaml for representing binary search
trees:

```ocaml
type tree =
  | Leaf
  | Node of int * tree * tree
```

This type definition specifies that a value of type `tree` is either a
leaf node, `Leaf`, or an internal node, `Node`, which consists of an
integer value, and two subtrees (the left and right subtree of the
node). `Leaf` and `Node` are referred to as the *variant constructors*
of the ADT `tree`.

The variant constructors `Leaf` and `Node` also serve as the value
constructors for type `tree`:

```ocaml
# let empty = Leaf ;;
val empty : tree = Leaf

# let t = Node (3, Node (1, Leaf, Leaf), Node (6, Leaf, Leaf)) ;;
val t : tree = Node (3, Node (1, Leaf, Leaf), Node (6, Leaf, Leaf))
```

One nice feature of ADTs is that equality `=` is defined structurally
on ADT values, similar to lists and tuples.

#### Pattern Matching

ADT values can be deconstructed using pattern matching. Pattern
matching expressions take the form:

```ocaml
match e with
| p1 -> e1
...
| pn -> en
```

Similar to match expressions in Scheme, this expression first
evaluates `e` and then matches the obtained value `v` against the
patterns `p1` to `pn`. For the first *matching* `pi -> ei` whose
pattern `pi` matches `v`, the right-hand-side expression `ei` is
evaluated. The value obtained from `ei` is then the result value of
the entire match expression. If no pattern matches, then a run-time
exception will be thrown.

The most important kinds of patterns `p` are:

* a constant literal pattern `c`: here `c` must be a constant literal
  such as `1`, `1.0`, `"Hello"`, `[]`, etc. The pattern `c` matches a
  value `v` iff `v` is equal to `c`.

* a wildcard pattern `_`: matches any value

* a variable pattern `x`: matches any value and binds `x` to that
  value in the right-hand-side expression of the matching.

* a constructor pattern `C p1`: here, `C` must be an ADT variant `C of
  t1` of some ADT and `p1` must be a pattern that matches values of
  type `t1`. Then `p` matches a value `v` if `v` is of the form `C v1`
  for some value `v1` matched by `p1`.
    
* a tuple pattern `(p1, ..., pn)`: matches a value `v` if `v` is a
  tuple `(v1, ..., vn)` where `v1` to `vn` are some values matched by
  the patterns `p1` to `pn`.

* a cons pattern `p1 :: p2`: matches values `v` that are lists of the
  form `v1 :: v2` where the head `v1` is matched by `p1` and the tail
  `v2` by `p2`.
  
* choice pattern `p1 | p2`: matches values `v` that are matched by
  pattern `p1` or pattern `p2`. Restriction: if a variable `x` occurs
  in `p1`, it must also occur in `p2` (and vice versa) and `x` must
  have the same type in both patterns.
  
* a variable binding pattern `(p1 as x)`: matches values `v` that are
  matched by `p1` and binds `x` to `v` in the right hand side of the
  matching.

Here is how we use pattern matching to define a function that checks
whether a binary search tree is sorted:

```ocaml
let is_sorted t = 
  let rec check min max t = 
    match t with
    | Leaf -> true
    | Node (x, left, right) ->
      min <= x && x <= max && 
      check min x left &&
      check x max right
  in
  check min_int max_int t
```

If we define a function whose body is a match expression that matches
the parameter of the function:

```ocaml
fun x -> match x with
| p1 -> e1
...
| pn -> en
```

and the parameter `x` is not used in any of the right hand sides of
the matchings `e1` to `en`, then this function definition can be
abbreviated to

```ocaml
function
| p1 -> e1
...
| pn -> en
```

We can thus write the function `check` inside of `is_sorted` more
compactly like this:

```ocaml
let check min max = function
| Leaf -> true
| Node (x, left, right) ->
  min <= x && x <= max && 
  check min x left &&
  check x max right
```

#### Polymorphic ADTs

ADT definitions can also be polymorphic. For instance, suppose we want
to use a binary search tree data structure to implement maps of
integer keys to values, but we want the data structure to be
parametric in the type of values stored in the map. This can be
done as follows:

```ocaml
type 'a tree =
  | Leaf
  | Node int * 'a * 'a tree * 'a tree
```

```ocaml
# let ti = Node (1, 2, Leaf, Leaf) ;;
val ti : int tree = Node (1, 2, Leaf, Leaf)

# let ts = Node (1, "banana", Leaf, Leaf) ;;
val ts : string tree = Node (1, "string", Leaf, Leaf)
```

##### Checking Exhaustiveness

One nice feature of OCaml's static type checker is that it can help us
ensure that pattern matching expressions are exhaustive. For instance,
suppose we write something like:

```ocaml
match t with
| Node (k, v, left, right) -> x
```

Then the compiler will warn us that we have not considered all
possible cases of values that `t` can evaluate to. The compiler will
even provide examples of values that we have not considered in the
match expression: in this case `Leaf`. This feature is particularly
useful for match expressions that involve complex nested patterns.

##### The `option` Type

One of the predefined ADTs of OCaml is the `option` type:

```ocaml
type 'a option =
  | None
  | Some of 'a
```

This type is useful for defining *partial* functions that may not
always have a defined return value. For instance, here is how we can
implement a function `find` that finds the value associated with a
given key `k` in a map implemented as a binary search tree, if such an
association exists:

```ocaml
let res find k = function
  | Node (k1, v, left, right) ->
    if k1 = k then Some v
    else if k1 > k then find k left
    else find k right
  | Leaf -> None
```

A client of `find` can now pattern-match on the result value of `find`
to determine whether the key `k` was present in the tree and what the
associated value `v` was in that case. The advantage of this
implementation is that the static type checker will check for us that
the client code will also consider the possibility that the key was
not found by `find`.

Contrast this with the use of `null` as an indicator of an undefined
return value in many other languages. The use of `null` values often
leads to run-time errors because the `null` case is not handled by the
client code and the compiler is unable to detect this at compile-time.

#### Tuples and Lists as ADTs

Note that both tuples and lists are just special cases of algebraic
data types. In particular, we can use list and tuple constructors in
patterns for pattern matching, like any other variant constructor of
an algebraic data type:

Example: reversing a list

```ocaml
let reverse xs = 
  let rec reverse_helper rev_xs = function
  | hd :: tl -> reverse_helper (hd :: rev_xs) tl
  | [] -> rev_xs
  in reverse_helper [] xs
  
# reverse [1; 2; 3];;
- : int list = [3; 2; 1]
```

Having tuples and lists as types that are built into the language is
just a matter of convenience. One could just as well implement lists
and tuples as user-defined types in a library. Here, is a user-defined
version of the type `'a list`:

```ocaml
type 'a mylist = 
  | Nil
  | Cons of 'a * 'a mylist
```

```ocaml
# let reverse xs = 
  let rec reverse_helper rev_xs = function
  | Cons (hd, tl) -> reverse_helper (Cons (hd, rev_xs)) tl
  | Nil -> rev_xs
  in reverse_helper Nil xs 
;;
val reverse: 'a mylist -> 'a mylist

# reverse (Cons (1, Cons (2, Cons (3, Nil))));;
- : int mylist = Cons (3, Cons (2, Cons (1, Nil)))
```

#### Pattern Guards

One restriction of patterns in OCaml is that a variable pattern `x` is
only allowed to occur at most once in a compound pattern. For
instance, lets look at the following literal translation of our
implementation of `removeDuplicates` from Scheme to OCaml:

```ocaml
let rec removeDuplicates = function
  | hd :: hd :: tl -> removeDuplicates (hd :: tl)
  | hd :: tl -> hd :: removeDuplicates tl
  | [] -> []
```

The compiler will complain with the following error for the first
matching in the definition of `removeDuplicates`:

```
Error: Variable hd is bound several times in this matching
```

We can avoid this problem by using different variable names for the
two occurrences of `hd` in the pattern of the first matching and then
enforce the equality of the values matched by these variables using a
*pattern guard*:

```ocaml
let rec removeDuplicates = function
  | hd1 :: hd2 :: tl when hd1 = hd2 -> removeDuplicates (hd2 :: tl)
  | hd :: tl -> hd :: removeDuplicates tl
  | [] -> []
```

The guard expression `hd1 = hd2` is evaluated after the pattern `hd1
:: hd2 :: tl` has been matched successfully. The right hand side
expression of the matching is then only evaluated if the guard
expression also evaluates to `true`. If the guard expression evaluates
to `false`, the next matching is tried. In general, any expression of
type `bool` can be used as a pattern guard.

Here is a slightly optimized version of the above implementation that
uses a variable binding pattern to avoid the allocation of a new cons
node for the tail list passed to the recursive call in the right hand
side of the first matching:

```ocaml
let rec removeDuplicates = function
  | hd1 :: (hd2 :: _ as tl) when hd1 = hd2 -> removeDuplicates tl
  | hd :: tl -> hd :: removeDuplicates tl
  | [] -> []
```

Note that `tl` is bound to the value matched by the pattern `hd2 :: _`.

#### Beyond Basic ADTs

OCaml also supports several generalizations of the basic ADTs
discussed here,
including
[generalized algebraic datatypes](https://caml.inria.fr/pub/docs/manual-ocaml/extn.html#sec252) and
[extensible variant types](https://caml.inria.fr/pub/docs/manual-ocaml/extn.html#sec266).


