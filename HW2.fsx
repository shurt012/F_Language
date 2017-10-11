    
/// 4. Correctness of sort recursive function with respect to the Checklist for Programming with Recursion
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
