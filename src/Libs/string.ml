(* string manipulation functions *)

let ord x = _ord x;
let chr x = _chr x;
let toUpper x = chr ((ord x) - 32); -- error checking needed
let toLower x = chr ((ord x) + 32);
