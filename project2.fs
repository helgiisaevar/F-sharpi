
//int that counts number of times a number is in a list. 

let f == 2 
let oldlist = []

for x in [0;7;4;3;5;7;7;3;8;6;17] do
    if f == x
    then(x)


let rec countOcc y xs =
    match xs with
    | [] -> 0
    | x::ys -> if x=y then 1 + countOcc y ys else countOcc y ys


//CODEthe same function using List.fold.

let countFold y xs = List.fold (fun n x -> if x=y then 1+n else n) 0 xs


//Code the same funciton using List.filter and List.length.


let countFilter y xs = List.length (List.filter ((=) y) xs);;



// daemi 2222222222


let occurs y xs = countOcc y xs> 0








module  Ex2Solutions

// SC-T-501-FMAL Programming languages, Practice class 2
// Spring 2020
// Solutions


// Problem 1 (i)


// countOcc directly by recursion

let rec countOcc y xs =
     match xs with
     | []    -> 0
     | x::xs -> if y = x then 1 + countOcc y xs else countOcc y xs


// using List.fold

// let countOcc y = List.fold (fun n x -> if y = x then 1 + n else n) 0

// using List.filter and List.length.

// let countOcc y xs = List.length (List.filter ((=) y) xs)


// (ii)

// occurs using countOcc

// let occurs y xs = countOcc y xs > 0

// using List.filter

// let occurs y xs = not (List.isEmpty (List.filter ((=) y) xs))

// directly by recursion

let rec occurs y xs =
    match xs with
    | []    -> false
    | x::xs -> y = x || occurs y xs

// using List.fold

// let occurs y xs = List.fold (fun b x -> y = x || b) false xs

// using List.exists

// let occurs y xs = List.exists ((=) y) xs


// occurs written with the help of countOcc always traverses the whole list,
// be countOcc coded directly by recursion or using List.fold,
// since countOcc must necessarily traverse the whole list to
// compute the count.

// occurs written by recursion or using List.exists only
// traverses the list up to the position where y is found.

// occurs coded with List.fold would be as efficient in a lazy language.
// But F# is eager, so (fun b x -> y = x || b) applied to some
// boolean expression b' always avaluates b'.
// So, in F#, occurs written with List.fold traverses the whole list.


// Problem 2

// sorted directly by recursion

let rec sorted xs =
    match xs with
    | [] | [_]         -> true
    | x::(x'::_ as xs) -> x <= x' && sorted xs


// using pairs and List.fold

let rec pairs xs =
    match xs with
    | [] | [_]         -> []
    | x::(x'::_ as xs) -> (x, x') :: pairs xs

// let sorted xs = List.fold (fun b (x, x') -> x <= x' && b) true (pairs xs)

// using pairs and List.forall

// let sorted xs = List.forall (fun (x, x') -> x <= x') (pairs xs)


// Problem 3 (i)

let rec removeFirst y xs =
    match xs with
    | []  -> []
    | x::xs -> if y = x then xs else x :: removeFirst y xs

// (ii)

let rec remove y xs =
    match xs with
    | []  -> []
    | x::xs -> if y = x then remove y xs else x :: remove y xs

// (iii)

let rec nub xs =
    match xs with
    | []  -> []
    | x::xs -> x :: nub (remove x xs)

// Problem 4 (i)

let rec suffixes xs =
    xs :: match xs with
          | []    -> []
          | _::xs -> suffixes xs


// (ii)

let rec prefixes xs =
    [] :: match xs with
          | []    -> []
          | x::xs -> List.map (fun ys -> x :: ys) (prefixes xs)

// (iii)

(*
let rec sublists xs =
    match xs with
    | [] -> [[]]
    | x::xs -> let yss = sublists xs
               List.map (fun ys -> x :: ys) yss @ yss
*)               

let rec sublists xs =
    match xs with
    | [] -> [[]]
    | x::xs -> List.collect (fun ys -> [x :: ys; ys]) (sublists xs)


// Problem 5 (i)

type 'a tree =
    | Lf
    | Br of 'a * 'a tree * 'a tree

let rootLabel t =
    match t with
    | Lf           -> 0      // leafs are not labelled, but we can pretend
                             // that they carry label 0
    | Br (n, _, _) -> n

let rec subtreeSums t =
    match t with
    | Lf          -> Lf
    | Br (a, l, r) ->
         let sl = subtreeSums l
         let sr = subtreeSums r
         Br (a + rootLabel sl + rootLabel sr, sl, sr)
            
// (ii)

let rec pathSumsAcc t acc =
    match t with
    | Lf           -> Lf
    | Br (a, l, r) ->
         let acc' = acc + a
         Br (acc', pathSumsAcc l acc', pathSumsAcc r acc')

let pathSums t = pathSumsAcc t 0

