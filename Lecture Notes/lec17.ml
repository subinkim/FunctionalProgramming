(* CS 4 2022-23, Prof. Vanier
  * Lecture 17 (3/1/2023)
  * Live Coding
*)

(* Continuation-Passing Style *)

(* multiplying all the numbers in the list *)
(* if 0 is somewhere in the list, doing multiplications until
   0 is inefficient
*)
let mult_loi list = 
  let rec aux lst =
    match lst with
    | [] -> 1
    | 0 :: _ -> raise Exit (* discards all the pending operations *)
    | h :: t -> h * aux t
  in
    try
      aux list
    with
      Exit -> 0

(* Deep CPS *)
let rec mult_loi_cps lst k =
  match lst with
  | [] -> k 1
  | 0 :: _ -> 0 (* discards all unnecessary evaluations *)
  | h :: t ->
    mult_loi_cps t
      (fun c -> k (h * c))
(* Not all parts of this use CPS - e.g. * and pattern-matching *)