(* HOF *)
let id x = x;
let const x y = x;
let flip f = \x y -> f y x;

(* boolean functions *)
let odd? x = if (x % 2 == 1) then true else false;
let even? x = if (x % 2 == 0) then true else false;

(* print functions *)
let show x = x;     (* this is just a synonym for id *)

(* maths function *)
let abs x = if 
    (x < 0) then (x * -1)
    else x;

let gcd' a b = 
    if (b == 0) then a
    else (gcd' b (a % b));

let gcd a b = gcd' (abs a) (abs b);
