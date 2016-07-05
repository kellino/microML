(* true and false as λcalculus *)

let tr x y = x;
let fls x y = y;

(* SKI combinators *)
let i x = x;        -- this is the same as id
let k x y = x;      -- same as constant
let s f g x = f x (g x);

let mu f = f (mu f);

let b x y z = x (y z);
let c x y z = x z y;
let w x y = x y y;
