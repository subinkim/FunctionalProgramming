(* CS 4 2022-23, Prof. Vanier
  * Recitation 1 (1/6/2023)
  * Live Coding
*)
let _ = Printf.printf "hello, CS 4 class!\n"

(* Lists *)
let lst = [1; 2; 3] (* ALL ELEMENTS MUST BE OF THE SAME TYPE *)
List.nth lst 1 (* Gives you an element at index 1, but we usually don't do this *)
List.hd lst (* Head of the Linked List *)
List.tl lst (* Tail of the Linked List; EVERYTHING BUT THE HEAD *)

(* Arrays *)
let arr = [|1; 2; 3; 4; 5|]
arr.(1)

(* Reference *)
let i = 0
i := 10 (* Returns an ERROR *)
let i = ref 0 (* Setting i equal to a reference with the content 0 *)
i := 10 (* Changes the value *)
!i (* Fetches the value that the reference points to *)

(* Type Variable *)
(* 'a = any type *)

(* String Concatenation *)
"foo" ^ "bar"

(* List Concatenation - old data doesn't change *)
let lst1 = [1; 2; 3]
let lst2 = [4; 5; 6]
List.append lst1 lst2 (* Method 1*)
lst1 @ lst2 (* Method 2 *)

(* Defining your own operator *)
let (++) lst1 lst2 = lst1 @ lst2
lst1 ++ lst2


let add1 = (+) 1
add1 10 (* returns 11 *)

(* Conditionals *)
let x = 15
if x > 10 then "bigger" else "smaller" (* must have the same type *)
if x > 10 then Printf "it's bigger!\n"

(* Functions *)
let double x = x * 2
let x = 10
double x (* returns 20 *)
let print_and_double x =
  begin
    Printf.printf "x = %d\n" x;
    x * 2
  end
print_and_double 10
(* OR *)
let print_and_double x =
  ( Printf.printf "x = %d\n" x;
    x * 2
)

(* Anonymous Functions - fun *)
let double = fun x -> x * 2
(fun x -> x * 2) 10
let f x y z = x + y * z
f 10 20 30
let f =  fun x y z -> x + y * z
let f = fun x -> fun y -> fun z -> x + y * z (* OCaml internally interprets the line above to this line *)
let f = fun x -> fun y -> fun (z : int) -> x + y * z (* Specifying types *)

(* Recursive Function - rec *)
let rec sum_to x = 
  match x with (* Pattern Matching *)
  | 0 -> 0
  | x' -> x' + sum_to (x' - 1)

let rec len lst = 
  match lst with
  | [] -> 0
  | _ :: t -> 1 + len t (* h :: t means head to tail of the linked list *)

let rec len lst = 
  match lst with
  | _ :: t -> 1 + len t 
  (* Gives warning because we didn't handle all the patterns (empty list pattern) *)
