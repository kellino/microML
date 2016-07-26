let x = []
let xs = [1,2,3]
let t xs = if xs == [] then 0 else 1
let id x = x

let map f xs =
    if xs == []
    then []
    else (f (head xs)) : (map f (tail xs))

let foldr f z xs =
    if xs == [] then z
    else (f x) : foldr f z (tail xs)

(* lambda *)
let doubled xs = map (\x -> x * 2) xs

let id'd = map id [1,2,3]
