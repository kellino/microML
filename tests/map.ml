let x = []
let xs = [1,2,3]
let t = if xs == [] then 0 else 1
(* map function *)
let map f xs =
    if xs == []
    then []
    else (f (head xs)) : (map f (tail xs))

(* foldr *)
let foldr f z xs =
    if xs == [] then z
    else (f x) : foldr f z (tail xs)

let doubled xs = map (\x -> x * 2) xs
