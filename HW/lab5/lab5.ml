(*
  Caltech WI 2022-23
  CS 4 Lab 5
  Rachael Kim
*)

(* Part A *)
(* A.1 *)
let fibonacci n =
  if n = 0 then n
  else
    let a = ref 0 in
    let b = ref 1 in
    let c = ref 1 in
    let counter = ref n in
    while !counter > 1 do
      c := !a + !b;
      a := !b;
      b := !c;
      counter := !counter - 1;
    done;
    !c

let fibonacci2 n =
  if n = 0 then n
  else
    let a = ref 0 in
    let b = ref 1 in
    let c = ref 1 in
    for _ = n - 1 downto 1 do
      c := !a + !b;
      a := !b;
      b := !c;
    done;
    !c


(* A.2 *)
let bubble_sort arr =
  let n = Array.length arr in
  if n <= 1 then ()
  else
    for i = 0 to n - 1 do 
      for j = 0 to n - 2 - i do
        if arr.(j) > arr.(j+1) then
          let temp = arr.(j) in
            arr.(j) <- arr.(j+1);
            arr.(j+1) <- temp;
      done;
    done


(* Part B *)
(* B.1 *)
let meters_per_foot = 0.3048

let get_meters len =
  match len with
    | `Meter m -> m
    | `Foot f -> f *. meters_per_foot
    | `Inch i -> i /. 12.0 *. meters_per_foot

let length_add a b = `Meter (get_meters a +. get_meters b)


(* B.2 *)
let grams_per_slug = 14593.903203
let get_grams mass =
  match mass with
    | `Gram m -> m
    | `Kilo k -> k *. 1000.0
    | `Slug s -> s *. grams_per_slug

let mass_add a b = `Gram (get_grams a +. get_grams b)

let get_seconds time =
  match time with
    | `Second s -> s
    | `Minute m -> m *. 60.0
    | `Hour h -> h *. 60.0 *. 60.0
    | `Day d -> d *. 24.0 *. 60.0 *. 60.0

let time_add a b = `Second (get_seconds a +. get_seconds b)


(* B.3 *)
let unit_add a b =
  match a, b with
    | `Length a, `Length b -> `Length (length_add a b)
    | `Mass a, `Mass b -> `Mass (mass_add a b)
    | `Time a, `Time b -> `Time (time_add a b)
    | _, _ -> invalid_arg "Arguments not are compatible"

(*
   Do we get into a combinatorial explosion when adding more unit 
   classes, at least as far as unit addition is concerned?

   No, we will not get into a combinatorial explosion. This is because
   adding one more unit class would only increase the number of cases 
   by 1, since the only cases we care about are when a and b have the
   same type. Every other cases will be dealt with _, _ base case.
   Thus, adding another unit class would ony increase the complexity
   linearly.
*)


(* Part C *)
(* C.1 *)
let rec make_gram g =
  let grams_per_slugs = 14593.903203 in
  let is_compatible unit_type = (unit_type = `Gram) || (unit_type = `Slug) in
    object
      method get_grams = g
      method get_slugs = g /. grams_per_slugs 
      method unit_type = `Gram
      method compatible other = is_compatible other#unit_type
      method add other = 
        if is_compatible other#unit_type then
          make_gram (other#get_grams +. g) (* THIS LINE *)
        else
          failwith "Invalid argument"
    end


(* C.2 *)
(* Define a number as a message-passing object. *)
(* "i" is an int. *)
let rec make_number i =
  object
    method value = i
    method show = string_of_int i
    method is_zero = i = 0
    method is_number = true
    method evaluate _ _ = make_number i  (* must evaluate to an object *)
    method derive _ = make_number 0  (* derivative of a number is 0 *)
  end

(* Define a variable as a message-passing object. *)
(* "v" is a string. *)
let rec make_variable v =
  object
    method value = failwith "variable has no numerical value"
    method show  = v
    method is_zero = false
    method is_number = false
    method evaluate v' n =
      if v = v'
        then make_number n
        else make_variable v
    method derive v' =
      if v = v'
        then make_number 1  (* d/dx(x) = 1 *)
        else make_number 0  (* d/dx(y) = 0 *)
  end

(* Define a sum as a message-passing object. *)
let rec make_sum expr1 expr2 =
  match () with
    | _ when expr1#is_zero -> expr2  (* 0 + expr = expr *)
    | _ when expr2#is_zero -> expr1  (* expr + 0 = expr *)
    | _ when expr1#is_number && expr2#is_number ->  (* add numbers *)
          make_number (expr1#value + expr2#value)
    | _ ->  (* create a new object representing the sum *)
          object
            method value = failwith "sum expression has no numerical value"
            method show = "(" ^ expr1#show ^ " + " ^ expr2#show ^ ")"
            method is_zero = false
            method is_number = false
            method evaluate v n =
              make_sum (expr1#evaluate v n) (expr2#evaluate v n)
            method derive v =
              make_sum (expr1#derive v) (expr2#derive v)
          end

(* C.2.a *)
let rec make_product expr1 expr2 =
  match () with
    | _ when expr1#is_zero -> make_number 0 
    | _ when expr2#is_zero -> make_number 0 
    | _ when expr1#is_number && expr1#value = 1 -> expr2
    | _ when expr2#is_number && expr2#value = 1 -> expr1
    | _ when expr1#is_number && expr2#is_number ->
        make_number (expr1#value * expr2#value)
    | _ ->  (* create a new object representing the sum *)
          object
            method value = 
              failwith "product expression has no numerical value"
            method show = "(" ^ expr1#show ^ " * " ^ expr2#show ^ ")"
            method is_zero = false
            method is_number = false
            method evaluate v n =
              make_product (expr1#evaluate v n) (expr2#evaluate v n)
            method derive v =
              make_sum (make_product (expr1#derive v) expr2)
                        (make_product expr1 (expr2#derive v))
          end

(* Evaluate a message-passing expression with a number
   substituted for a variable. *)
let evaluate expr v n = expr#evaluate v n

(* Return the string representation of an expression. *)
let show expr = expr#show

(* Return the derivative of an expression. *)
let differentiate expr v = expr#derive v


(* C.2.b *)
(* C.2.b.i *)
(*
val f :
  < derive : string -> 'a; evaluate : string -> int -> 'a; is_number : 
    bool; is_zero : bool; show : string; value : int >
  as 'a = <obj>
*)


(* C.2.b.ii *)
(*
val dfdx :
  < derive : string -> 'a; evaluate : string -> int -> 'a; is_number : 
    bool; is_zero : bool; show : string; value : int >
  as 'a = <obj>
*)


(* C.2.b.iii *)
(*
- : string =
"(((x * (x * y)) + (x * ((x * y) + (x * y)))) + (3 * ((x * (y * y)) + (x * (y * y)))))"
*)


(* C.2.b.iv *)
(*
- : string =
"((3 * (3 * (3 * y))) + ((3 * (3 * (3 * (y * y)))) + ((y * y) + 2)))"
*)


(* C.2.b.v *)
(*
- : string = "558"
*)


(* C.2.b.vi *)
(*
- : string = "396"
*)