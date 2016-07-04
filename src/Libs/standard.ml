(* HOF *)
let id x = x;
let const x y = x;
let flip f = \x y -> f y x;
let ap f x = f (f x);


(* standard list functions *)
-- head is a builtin
-- tail is a builtin
-- cons is a builtin
let length xs = if (xs == []) then 0 else (1 + (length (tail xs)));

(* print functions *)
let show x = x;     (* this is just a synonym for id *)

(* maths function *)
let abs x = if (x < 0) then (x * -1) else x;
let max a b = if (a < b) then b else a;
let min a b = if (a < b) then a else b;
let square a = a * a;
let negate x = if (x < 0) then (abs x) else (x * -1);

-- tests --
let zero? x = if (x == 0) then true else false;
let odd? x = if (x % 2 == 1) then true else false;
let even? x = if (x % 2 == 0) then true else false;

-- we need to have lets and where to hide definitions
let gcd' a b = 
    if (b == 0) then a
    else (gcd' b (a % b));

let gcd a b = gcd' (abs a) (abs b);
