// 1
let rec inner xs ys = 
    match (xs, ys) with
    | ([x],[y]) -> x*y
    | (x::xs, y::ys) -> x*y + inner xs ys
    | (_, _)-> failwith "inner defined for equal sized lists only"



// 2 using transpose from last HW
let rec transpose = function
    | (_::_)::_ as M -> List.map List.head M :: transpose (List.map List.tail M)
    | _ -> []

// Posible to improve this by transposing beforehand
let rec multiply = function
|([], _) -> []
|(x::xs, ys) -> List.map (inner x) (transpose ys) :: multiply (xs, ys)
    
// 3
let flatten1 xs = List.fold (@) [] xs
let flatten2 xs = List.foldBack (@) xs []
let makelistlist n = List.map (fun x -> [x]) [1..n]
#time
(*In practice flatten1 behaves as O(n^2) and flatten2 is (O(n))*)

// 4
(*
    twice successor 0 would be 2 = 2^1
    twice twice successor 0 would be 4 = 2^2
    twice twice twice successor 0 would be 16 = 2^4
    And so on..

    The current value becomes equals to 2 to the power of the previous value

    The function will be defined recursively, as follows:
        f(n) = 
            if n = 0 then 1
            else 2 ^ (f(n-1))

    We can write this function as:
    let rec f = function
        | 0 -> 1.0
        | n -> 2.0 ** (f (n-1));;
*)

// 5
type 'a stream = Cons of 'a * (unit -> 'a stream);;

let rec map f (Cons(x, xsf)) = Cons (f x, fun() -> map f (xsf()));;

// 6
type Exp = Num of int
         | Neg of Exp
         | Sum of Exp * Exp
         | Diff of Exp * Exp
         | Prod of Exp * Exp
         | Quot of Exp * Exp

let rec evaluate = function
| Num n -> Some n
| Neg e -> match evaluate e with
           | None -> None
           | Some n -> Some (-1 * n)
|Sum (e1, e2) -> match (evaluate e1, evaluate e2) with
                 |(None, _) -> None
                 |(_, None) -> None
                 |(Some n1, Some n2) -> Some (n1 + n2)
|Diff (e1, e2) -> match (evaluate e1, evaluate e2) with
                  |(None, _) -> None
                  |(_, None) -> None
                  |(Some n1, Some n2) -> Some (n1 - n2)
|Prod (e1, e2) -> match (evaluate e1, evaluate e2) with
                  |(None, _) -> None
                  |(_, None) -> None
                  |(Some n1, Some n2) -> Some (n1 * n2)
|Quot (e1, e2) -> match (evaluate e1, evaluate e2) with
                  |(None, _) -> None
                  |(_, None) -> None
                  |(_, Some 0) -> None
                  |(Some n1, Some n2) -> Some (n1 - n2)
