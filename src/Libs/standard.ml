let zero? x = if x == 0 then true else false;
let odd? x = if x % 2 == 1 then true else false;
let even? x = if x % 2 == 0 then true else false;
let positive? x = if x > 0 then true else false; 
let negative? x = if x < 0 then true else false;
--let end? xs = if xs == [] then true else false;

let show x = x; 

let id x = x;
let const x y = x;
let succ x = x + 1;
let flip f = \x y -> f y x;
let twice f x = f (f x);
--let cons a b = a : b;
let pipe x f = f x;
let compose f g x = f (g x);
--let length xs = if end? xs then 0 else 1 + (length (tail xs));
--let drop n xs = if (zero? n) then xs else (drop (n-1) (tail xs));
--let take n xs = if (zero? n) then ([]) else ((head xs) : (take (n-1) (tail xs)));

--let foldr f acc xs = if (end? xs) then acc else ((f (head xs)) (foldr f acc (tail xs)));
--let foldl f acc xs = if (end? xs) then acc else (foldl f (f acc (head xs)) (tail xs));
--let foldr1 f xs = foldr f (head xs) xs;
--let foldrl1 f xs = foldl f (head xs) xs;

--let map f xs = foldr (\x xs' -> (f x) : xs') ([]) xs;
--let filter p xs = foldr (\x y -> if (p x) then (x:y) else y) [] xs;
--let concat xs
--let concatMap xs
--let until p f x = if (p x) then x else (until p f (f x));

