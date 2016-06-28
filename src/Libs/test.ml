(* Functions *)
let const x y = x;
let compose f g = \x -> f (g x);
let twice f x = f (f x);
let on g f = \x y -> g (f x) (f y);
let ap f x = f (f x);

(* Let Polymorphism *)
let self = (\x -> x) (\x -> x);
let innerlet = \x -> (let y = \z -> z in y);
let innerletrec = \x -> (let rec y = \z -> z in y);

(* Fresh variables *)
let wtf = \a b c d e e' f g h i j k l m n o o' o'' o''' p q r r' s t u u' v w x y z ->
    q u i c k b r o w n f o' x j u' m p s o'' v e r' t h e' l a z y d o''' g;

let until p f x = 
  if (p x)
  then x
  else (until p f (f x));
