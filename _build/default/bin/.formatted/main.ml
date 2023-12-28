open Math

let rec len lst = match lst with [] -> 0 | _ :: t -> 1 + len t

(* is coprime *)
(* print_endline (string_of_bool (is_coprime 20536 7826));; *)

(* euler's totient function *)
let phi e =
  match e with
  | 0 | 1 -> 1
  | _ -> len (List.filter (fun x -> x = 1) (List.map (gcd e) (range 0 (e + 1))))
