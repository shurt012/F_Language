//Problem 1:
let rec gcd = function
  |(a,0)->a
  |(a,b)-> gcd(b, a % b)

//Adding two fractions together
let (.+) (a,b)(c,d) =
    let numerator = (a*d)+(c*b) //cross multiplies the numerators
    let denominator = b * d  //Multiplies the denominator
    let getGCD = gcd(b,d) //Finds common denominator between the two fractions
    ((numerator/getGCD),(denominator/getGCD));; //Simplies final result

 (*
  
  (1,2) .+ (1,3);;
   val it : int * int = (5, 6)
 *)

 //Multiplies two fractions together
 let(.*) (a,b)(c,d) =
    let numerator = a * c //Multiply the numerators
    let denominator = b * d //Multiply the denominators
    let getGCD = gcd(numerator,denominator) // Find comming denominator 
    ((numerator/getGCD),(denominator/getGCD));; //Simplifies final result

    (*
     (1,2) .+ (2,3) .*(3,7);;
      val it : int * int = (11, 14)
    *)
     

 //Problem 2
let revList xs = List.map(List.rev) xs;;
 (*
  revList [[1;2;3];[5];[6;7;8]];;
   val it : int list list = [[3; 2; 1]; [5]; [8; 7; 6]]

  revList [['a';'b';'c'];['d'];['e';'f';'g']];;
   val it : char list list = [['c'; 'b'; 'a']; ['d']; ['g'; 'f'; 'e']]
 *)

 //Problem 3
let rec interleave = function
   |(xs,[]) -> xs
   |([],ys) -> ys
   |(x::xs, y::ys) -> x::y::interleave(xs,ys);;

   (*
   interleave ([1;2;3],[4;5;6]);;
    val it : int list = [1; 4; 2; 5; 3; 6]
    *)

 //Problem 4
let gencut (n, xs) =
    let rec splitNList = function
      |0, list1, list2 -> ( List.rev list1,list2)
      |n, list1, [] -> (list1,[])
      |n, list1, list2 -> splitNList(n-1,  List.head list2::list1, List.tail list2)
    splitNList(n,[], xs);;

 let cut xs = gencut((List.length xs)/2, xs);;

 (*
 cut [1..6];;
  val it : int list * int list = ([1; 2; 3], [4; 5; 6])
 *)

 //Problem 5
 let shuffle xs =  interleave(cut xs);;
 (*
  shuffle [1..8];;
   val it : int list = [1; 5; 2; 6; 3; 7; 4; 8]
 *)

 //Problem 6
 let countshuffles = 
   let countaux(deck, target) = 
