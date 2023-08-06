(* CS 4 2022-23, Prof. Vanier
  * Recitation 2 (2/1/2023)
  * Live Coding
*)


(* Algebraic Data Types *)
type card = Spade | Heart | Diamond | Club

(*
   Capitalization conventions:
   - type name is not capitalized
   - constructors are capitalized
*)

let string_of_card c =
  match c with
  | Spade -> "Spade"
  | Heart -> "Heart"
  | Diamond -> "Diamond"
  | Club -> "Club"

let compare_cards c1 c2 =
  match (c1, c2) with
  | (Spade, Spade) -> 0
  | (Spade, Heart) -> -1
  | ... All the cases

type number = 
  | Zero
  | Integer of int
  | Real of float

let float_of_number = function
  | Zero -> 0.0
  | Integer i -> float_of_int i
  | Real r -> r

(* recursive datatype *)
type nat = 
  | Z
  | S of nat

(* tuples *)
(* no direct way to access values at nth position *)
(* you need to pattern match *)

(* records *)
type named_point = {
  name: string;
  x: float;
  y: float;
}
(* u can get these values by doing .name, .x, .y *)
(* by adding mutable keyword, you can also make these mutable *)

let add_points p1 p2 =
  match (p1, p2) with
  | ({name=n1; x=x1; y=y1},{name=n2; x=x2; y=y2}) -> {name=n1; x=x1+.x2; y=y1+.y2}

let add_points p1 p2 =
  let ({name=n1; x=x1; y=y1},{name=n2; x=x2; y=y2}) = (p1,p2) in 
    {name=n1; x=x1+.x2; y=y1+.y2}

let add_points {name=n1; x=x1; y=y1} {name=n2; x=x2; y=y2} =
  {name=n1; x=x1+.x2; y=y1+.y2}

let add_points p1 p2 =
  {name=p1.n1; x=p1.x +. p2.x; y= p1.y +. p2.y}

let rec list_length = function
  | [] -> 0
  | _ :: t -> 1 + list_length t

module L = List (* abbreviating module names *)

(* exceptions *)
exception Bad of string
exception Awful
exception WrongOnSoManyDifferentCounts of string * string * int
(* much like ADT *)

(* built in exceptions *)
Failurue "bad"
Invalid_argument "bad arg"
raise (Failure "bad")
invalid_arg "bad arg"

let rec factorial n =
  match n with 
  | _ when n < 0 -> raise (Invalid_argument "factorial")
  | 0 -> 1
  | _ -> n * factorial (n - 1)

let rec find x lst =
  match lst with
  | [] -> raise Not_found
  | h :: t -> if h = x then x else find x t

(* Catching an exception *)
try
  find 4 [1;2;3]
with Not_found -> 0

(* we can also do pattern match on exception handlers to have multiple exceptions *)