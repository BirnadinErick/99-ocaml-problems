(* returns greatest common divisor of both p and q *)
let rec gcd p q =
  if q = 0 then p
  else
    let r = p mod q in
    gcd q r

(* exercise: find coprime of positive ints *)
let is_coprime p q = gcd p q = 1

(* returns list of range of ints between a and exclusive b*)
let rec range a b = if a = b then [] else a :: range (a + 1) b
