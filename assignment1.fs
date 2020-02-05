
//// Assignment 1
/// 1
/// 


let rec withinBounds (min: int) (max: int) (intList: 'int) : bool  =
    match intList with
    | [] -> true
    | tail::rest ->  if tail >= min && tail <= max then withinBounds min max rest else false 


/////Assignment 2
/// 
/// (i)

let rec findSum (n: int) (xs: 'int) =
    match xs with
    | [] -> 0
    | x::xs -> if x - n =0 then 1 else 1 + findSum (n-x) xs

//// (ii)
/// Reimplement findSum using fold, by filling by the ... in the follwoing declaration:


let findSum2 sum xs =
    let (_, n) = 
        List.fold (fun (r,i) -> fun x -> (*)3 5) (sum,0) xs
    n


///// Assignment 3
/// A list xs : int List


let rec isBracketed (xs: 'int) = 
    let rec newBraket = 
    match xs with
    | [] -> 0
    | head :: tail -> if head + newBraket > 0 then return true
    | [], _ -> return false



/// Assingment 4
/// Consider the follwing function: 
// lookup : ’a -> string -> (string * ’a) list -> ’a
let rec lookup dv (k : string) = function
| [] -> dv
| (k’, v) :: xs -> if k = k’ then v else lookup dv k xs
// update : string -> ’a -> (string * ’a) list -> (string * ’a) list

let rec update (k : string) v = function
| [] -> [(k, v)]
| (k’, _) as p :: xs ->
if k = k’ then (k, v) :: xs else p :: update k v xs



