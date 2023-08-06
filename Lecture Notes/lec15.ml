(* CS 4 2022-23, Prof. Vanier
  * Lecture 15 (2/24/2023)
  * Live Coding
*)

(* Streams and Lazy Evaluations *)

(* Lazy module *)
type 'a stream =
  | Nil
  | Cons of 'a * 'a stream lazy_t (* lazy_t = deferred evaluation *)

let x = lazy (2 + 2) (* putting 2 + 2 in a function so it's only evaluated when needed *)

Lazy.force x (* forces lazy value to be evaluated*)

(* Stream *)
(* made up of a series of expressions, until the stream ends with Nil or it goes on forever *)
let stream_head = function
  | Nil -> failwith "head of an empty stream"
  | Cons (h, _) -> h

let stream_tail = function
  | Nil -> failwith "tail of an empty stream"
  | Cons (_, t) -> force t (* to get a tail, we need to force it *)

let rec stream_nth s n =
  if n = 0 then stream_head s
  else stream_nth (stream_tail s) (n - 1)

let rec stream_map f s =
  match s with
    | Nil -> Nil
    | Cons (h, t) ->
      Cons (f h, lazy (stream_map f (force t)))

let rec stream_enumerate_interval low high =
  if low > high then Nil
  else Cons (low, lazy (stream_enumerate_interval (low + 1) high)) (* adding lazy saves so much space, because we are not evaluating it unless we need to *)

let rec integers_starting_from n =
  Cons (n, lazy (integers_starting_from (n + 1)))

let rec fibgen a b = 
  Cons (a, lazy (fibgen b (a + b)))

let rec stream_drop_while pred s =
  if pred (stream_head s) then stream_drop_while pred (stream_tail s)
  else s

let add_streams s1 s2 = stream_map (+) s1 s2

(* Lazy definition of a data structure, it will be a function at some part of its evaluation *)
let rec integers' = Cons (1, lazy (add ones integers'))

(* We can also create a stream of streams *)