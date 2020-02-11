
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
        List.fold (fun (r,i) -> fun x -> if r=0 then (r,i) else (r-x,i+1)) (sum,0) xs
    n


///// Assignment 3
/// A list xs : int List

let isBracketed (Listi: int list) = 
    let rec newBraket xs n =
        match (xs, n) with
            | [], 0 -> true
            | head :: tail, _ ->  if  n + head < 0 then false else newBraket tail (n+head)
    newBraket Listi 0 



/// Assingment 4
/// Consider the follwing function: 
// lookup : ’a -> string -> (string * ’a) list -> ’a

// EKKI Búinn med þetta verkefni!!!!


let rec lookup dv (k : string) = function
| [] -> dv
| (k’, v) :: xs -> if k = k’ then v else lookup dv k xs

// update : string -> ’a -> (string * ’a) list -> (string * ’a) list
let rec update (k : string) v = function
| [] -> [(k, v)]
| (k’, _) as p :: xs ->
if k = k’ then (k, v) :: xs else p :: update k v xs


/// (i)


let rec count (item: string) (Listitem: string list) : int = 
    match Listitem with
    | [] -> 0
    | x::xs -> if item = x then 1 + count item xs else count item xs



// modify : ’a -> (’a -> ’a) -> string -> (string * ’a) list -> (string * ’a) list
let rec modify d f x xs = update x (f (lookup d x xs)) xs

//(ii)


let rec modify d f x xs =
    match xs with 
    | [] -> [(x, f d)]
    | (b,h) as p :: xs -> 
        if x = b then (x, f h) :: xs else p :: modify d f x xs


//(iii)


///Assignment 5 
let rec uf f x =
match f x with
    | None -> []
    | Some (a, y) -> a :: uf f y

let fromOne (n : int) = uf (fun x -> if (n>=x) then Some(x,x+1) else None) 1

/// Assingment 6 

type ’a tree =
    | Lf
    | Br of ’a * ’a tree * ’a tree
type pos =
    | S // stop here
    | L of pos // go left
    | R of pos // go right


let rec deleteSubtree (t : 'a tree) (p : pos) : 'a tree =
    match p, t  with
    | S, t -> Lf
    | L p, Br (n, t1, t2) -> Br (n, deleteSubtree t1 p, t2)
    | R p, Br (n, t1, t2) -> Br (n, t1, deleteSubtree t2 p)
    | R p, Lf -> Lf
    | L p, Lf -> Lf

/// Assignemnt 7 

type fp = 
    | Nil 
    | IntCons of int * fp
    | StrCons of string * fp 

//(i)

let rec  fromIntList (Listi : int list ) fp = 
    match Listi with
    | [] -> Nil 
    | x :: tail fromIntList(List, tail)

// (ii)

