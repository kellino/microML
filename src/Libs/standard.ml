(* tests *)
let zero? x = if (x == 0) then true else false;
let odd? x = if (x % 2 == 1) then true else false;
let even? x = if (x % 2 == 0) then true else false;
let positive? x = if (x > 0) then true else false;
let negative? x = if (x < 0) then true else false;
let end? xs = if (xs == []) then true else false;

(* HOF *)
let id x = x;
let const x y = x;
let flip f = \x y -> f y x;
let twice f x = f (f x);
let cons a b = a : b;
let length xs = if (end? xs) then 0 else (1 + (length (tail xs)));
let drop n xs = if (zero? n) then xs else (drop (n-1) (tail xs));
let take n xs = if (zero? n) then ([]) else ((head xs) : (take (n-1) (tail xs)));
let foldr f acc xs = if (end? xs) then acc else ((f (head xs)) (foldr f acc (tail xs)));
let foldl f acc xs = if (end? xs) then acc else (foldl f (f acc (head xs)) (tail xs));
let map f xs = foldr (\x xs' -> (f x) : xs') ([]) xs;
let filter p xs = foldr (\x y -> if (p x) then (x:y) else y) ([]) xs;

(* print functions *)
let show x = x;     (* this is just a synonym for id *)

(* maths function *)
--let sum xs = if (xs == []) then 0 else ((head xs) + (sum (tail xs)));
let sum xs = foldl (\x y -> x + y) 0 xs;
-- let product xs = if (xs == []) then 1 else ((head xs) * (product (tail xs)));
let product xs = foldl (\x y -> x * y) 1 xs;
let abs x = if (negative? x) then (x * -1) else x;
let max a b = if (a < b) then b else a;
let min a b = if (a < b) then a else b;
let square a = a * a;
let negate x = if (negative? x) then (abs x) else (x * -1);

-- we need to have lets and where to hide definitions
let gcd' a b = if (b == 0) then a else (gcd' b (a % b));
let gcd a b = gcd' (abs a) (abs b);
