(* maths standard library *)


-- constants
let pi = 3.14159265359;
let e  = 2.718281828459;
let square a = a * a;
-- naive fibonacci
let fib n = if (zero? n) 
    then 1 
    else if (n == 1)
    then 1
    else ((fib (n-1)) + (fib (n-2)));
