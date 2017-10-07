(* Homework 2

*)

/// 1. Returns a list of pairs that represents the cartesian product
let rec cartesian(xs, ys) =
    match xs, ys with
    | (xs, []) -> []
    | ([], ys) -> []
    | (x::xs, ys) -> (List.map(fun y -> x,y) ys) @ (cartesian (xs,ys));;

/// 2. Powerset set returns the set of all subsets of set
let rec powerset = function
    | [] -> [[]]
    | x::xs -> List.collect (fun subset -> [subset; x::subset]) (powerset xs)

/// 3. Transpose an m-by-n matrix
let rec transpose = function
    | (_::_)::_ as M -> List.map List.head M :: transpose (List.map List.tail M)
    | _ -> []
    
/// 4. Correctness of sort recursive function with respect to the Checklist for Programming with Recursion
(*
Step One:
Step Two:
Step Three:
*)
let rec sort = function
  | []         -> []
  | [x]        -> [x]
  | x1::x2::xs -> if x1 <= x2 then x1 :: sort (x2::xs)
                              else x2 :: sort (x1::xs)

/// 5. Analyze mergesort with respect to the Checking for Programming with Recursion (merge and split work correctly)
(*
Step One:
Step Two:
Step Three:

Clue something is wrong: The mergesort function is missing a case, which causes mergesort to be seen by 
the compiler as 'a list -> 'b list instead of 'a list -> 'a list
*)
let rec merge = function
  | ([], ys)       -> ys
  | (xs, [])       -> xs
  | (x::xs, y::ys) -> if x < y then x :: merge (xs, y::ys)
                               else y :: merge (x::xs, ys)

let rec split = function
  | []       -> ([], [])
  | [a]      -> ([a], [])
  | a::b::cs -> let (M,N) = split cs
                (a::M, b::N)

let rec mergesort = function
  | []  -> []
  | [x] -> [x]  //Bug corrected
  | L   -> let (M, N) = split L
           merge (mergesort M, mergesort N)

/// 6. Define F# function curry f that converts an uncurried function to a curried function, and 
(* an F# uncurry f that does the opposite conversion *)
let curry f a b = f (a, b)
let uncurry f (a,b) = f a b

(*
val curry : ('a * 'b -> 'c) -> 'a -> 'b -> 'c
val uncurry : ('a -> 'b -> 'c) -> 'a * 'b -> 'c
*)