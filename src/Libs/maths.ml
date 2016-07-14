(* maths standard library *)

-- constants
let pi = 3.14159265359;
let e  = 2.718281828459;

let abs x     = if x > 0 then x else x * (-1);
let negate x  = if x < 0 then abs x else x * -1;
--let sum xs  = foldl (\x y -> x + y) 0 xs;
--let product xs = foldl (\x y -> x * y) 1 xs;
let max a b    = if a < b then b else a;
let min a b    = if a < b then a else b;

(* conversions *)
let intToFloat x = x + 0.0;

let reciprocal x = 1 / x;
let square a = a * a;
let sqrt x = x^0.5;
let floor x = x // 1;
let ceiling x = 1 + (floor x);

(* log functions *)
let ln x        = _log x + 0; -- a bug this, but (+0) is needed to force inference of Number type
let log2 x      = _log x / _log x;
let log10 x     = _log x / _log 10;
let logBase x y = _log y / _log x;

-- naive fibonacci
let fib n = if n == 0
    then 1 
    else if n == 1
    then 1
    else (fib (n-1)) + (fib (n-2));

let gcd a b = if a == b
    then a
    else if a < b
    then gcd b a
    else gcd b (a-b);

