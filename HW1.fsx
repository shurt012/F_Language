(*
Jose Gabriel Perez
ex 1: could save gcd on reduce to avoid double calc on big nums
Also, finding common denom instead to reduce computing could be better for large numbers
*)

let rec gcd = function
| (a,0) -> a
| (a,b) -> gcd (b, a % b)

let reduce = function
| (a, b) -> (a/gcd(a,b), b/gcd(a,b))

let (.+) (a,b) (c,d) = reduce (a*d + b*c, b*d)
let (.*) (a,b) (c,d) = reduce (a*c, b*d)

//ex2
let revlists l = List.map List.rev l

//ex3 Really banking on l1 and l2 having equal length
let interleave (l1, l2) =  
    let rec interHelp (r, l1, l2) = 
        match l1 with
        | [] -> r
        | x::xs -> interHelp (r @ (x::List.head l2::[]) , xs, List.tail l2)
    interHelp([], l1, l2)

//ex4
let gencut(n, l1) = 
    let rec helpcut (n, a, b) = 
        if List.isEmpty b then (n, a, b)
        elif List.length a < n then helpcut (n, a @ (List.head b :: []), List.tail b)
        else (n, a, b)
    let (_, f1, f2) = helpcut (n, [], l1)
    (f1, f2)

let cut l1 = gencut((List.length l1)/2, l1)

//ex5
let shuffle l = interleave (cut l)

//ex6
let countshuffles n = 
    let rec countaux(deck, target) =
        if deck = target then 0
        else 1 + countaux(shuffle deck, target)
    countaux(shuffle [1..n],[1..n]) + 1

//samples
interleave ([1;2;3],[4;5;6]);;
cut [1;2;3;4;5;6];;
shuffle [1;2;3;4];;
countshuffles 8;;
countshuffles 52;;

shuffle [1; 2; 3; 4; 5; 6; 7; 8];;
shuffle [1; 5; 2; 6; 3; 7; 4; 8];;
shuffle [1; 3; 5; 7; 2; 4; 6; 8];;