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
    
//// 4. Correctness of sort recursive function with respect to the Checklist for Programming with Recursion
(*
Step One: It is okay, because empty and single item lists are automatically sorted
Step Two: wrong, because even assuming that sort (x2::xs) returns the correct answer, 
  there is no guarantee that x1 is the minimun since it was only compared to x2 and 
  is not checked again. For a counter example sort [3;2;1];; will return [2;1;3;]. 
  All that this sort function really guarantees is that the largest element goes to the 
  end of the list. 
  (Note: Much like insertion sort, if it gets called n-1 times the list would be sorted)
Step Three: Each recursive call gets an input that is smaller than the original input
*)
let rec sort = function
  | []         -> []
  | [x]        -> [x]
  | x1::x2::xs -> if x1 <= x2 then x1 :: sort (x2::xs)
                              else x2 :: sort (x1::xs)

/// 5. Analyze mergesort with respect to the Checking for Programming with Recursion (merge and split work correctly)
(*
Step One: It is okay, because empty lists are automatically sorted
Step Two: Non base case did not previously exist. Added as | [x] -> [x] in order to correct this
Step Three: Each recursive call gets an input that is smaller than the original input
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

