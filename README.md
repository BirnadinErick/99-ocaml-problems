# OCaml Exercises

#hub/ocaml  #hub/cs #hub/dsa #hub/algebra 

Following are coding practice while engaging in [OCaml's Execersises](https://ocaml.org/exercises)

> Find length of a list

```ocaml
let rec len lst = match lst with [] -> 0 | _ :: t -> 1 + len t
```

---

> Generate list of positive integer range $[a, b)$

```ocaml
let rec range a b = if a = b then [] else a :: range (a + 1) b
```

---

> Find GCD using Euclid's method [[euclid-algo]] [Exercise 32](https://ocaml.org/exercises#32) #intermediate 

```ocaml
let rec gcd p q =
  if q = 0 then p
  else
    let r = p mod q in
    gcd q r
```

---

> Check whether 2 positive integers are Co-prime [Exercise 33](https://ocaml.org/exercises#33) #beginner

```ocaml
let is_coprime a b = (gcd a b) = 1
```

---

> Find Euler's totient $\Phi(n)$  [Exercise 34](https://ocaml.org/exercises#34) #intermediate  

```ocaml
let phi n =
  match n with
  | 0 | 1 -> 1
  | _ -> len (List.filter (fun x -> x = 1) (
		  List.map (gcd n) (range 1 (n + 1))
	))
```

---

> get last element of a list #beginner 

```ocaml
let rec last lst = match lst with
	| [] -> None
	| t :: [] -> Some t
	| _ :: u -> last u
	;;
```

---

> get last *two* elements of a list #beginner 

```ocaml
let rec last2 xs = match xs with
	| [] | [_] -> None
	| [t; t'] -> Some (t, t')
	| _ :: xss -> last2 xss
	;;
```

---

> Find Nth element #beginner 

```ocaml
(** here indeces gives list of indices of given list *)
let nth j lst = match ( last (List.filter (function (_, i) -> i = j) (List.combine lst (indices lst)))) with
| None -> raise (Failure "nth")
| Some (x,_) -> x;;
```

- given solution in the web-page

```ocaml
let nth k = function
	| [] -> None
	| h :: t -> if k = 0 then (Some h) else at (k-1) t
	;;
```

---

> reverse a list #beginner **did not complete**

```ocaml
let rev list = 
	let rec aux acc = function
		| [] -> acc
		| h :: t -> aux (h :: acc) t
	in
	aux [] list
;;
```

 ☠️ formatting messes up code, click to reveal correct version $| h::t -> aux\  (h::acc)\  t$ 👆
 
---

> convert string to character list #intermediate 

```ocaml
let string_to_list s = 
	let rec aux i acc = 
		if i < 0 then acc
		else aux (i-1) (s.[i] :: acc)
	in aux (String.length s - 1) []
;;
```

---

> check if list is palindrome #beginner 

```ocaml
let is_palindrome xs = xs = List.rev xs;;
```

- enabling direct string

```ocaml
let is_palindrome str = 
	let xs = string_to_list str in
	xs = List.rev
;;
```

---

> duplicate the elements in list #beginner 

```ocaml
let duplicate list = 
	let aux acc i = i :: (i :: acc)
	in List.rev (List.fold_left aux [] list)
```

---


