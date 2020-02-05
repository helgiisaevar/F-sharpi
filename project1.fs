



// f(n) = n * 3 + 8 

let f n = n * 3 + 8;;

let f = fun n -> n * 3 + 8;;




// assignment 2. pow2(n) = 1 if n==0
// 2* pow2(n-1 ) otherwise

(*
let pow2 = fun n -> if n = 0 then 1 else 2 * pow2 (n - 1);;
Here SML complains that the pow2 in the rhs is not in scope.
*)



let rec pow2 = fun n -> if n=0 then 1 else 2 * pow2 (n-1)


let rec pow2 n =
    match n with
    | 0 -> 1
    | _ -> 2 * pow2 (n - 1);;


//Problem 3 

let rec mc n = if n > 100 then n - 10 else mc (mc (n + 11));;

//Problem 4

let rec feq h k n m = 
    if n > m then true else h n = k n && feq h k (n+1) m;;

//Problem 5


