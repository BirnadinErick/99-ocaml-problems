open Math

let rec len lst = match lst with [] -> 0 | _ :: t -> 1 + len t;;

(* is coprime *)
 print_endline (string_of_bool (is_coprime 20536 7826));;

(* euler's totient function *)
let phi e =
  match e with
  | 0 | 1 -> 1
  | _ -> len (List.filter (fun x -> x = 1) (List.map (gcd e) (range 1 (e + 1))));;

print_endline (string_of_int (phi 9));;

(* is_palindrome *)
let str_to_list s = 
  let rec aux i acc = 
    if i < 0 then acc
    else aux (i-1) (s.[i]::acc)
  in aux (String.length s - 1) []
;;

let is_palindrome s = 
  let xs = str_to_list s in
  xs = List.rev xs
;;

print_endline (string_of_bool (is_palindrome "xamax"));;

(* run length encoding *)
let create_board map k = if Hashtbl.mem map k then Hashtbl.add map k ((Hashtbl.find map k) + 1)
  else Hashtbl.add map k 0
;;

let run_encode s = 
  let board = Hashtbl.create (String.length s) in
  let xs = str_to_list s in
  List.map (fun x -> create_board board x) xs
;;
