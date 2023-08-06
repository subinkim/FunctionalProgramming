(* CS 4 2022-23, Prof. Vanier
  * Recitation 4 (2/22/2023)
  * Live Coding
*)

(* Modules and Functors *)

(* Interface vs Implementation *)
(* Interface in .mli, Implementation in .ml *)
(* we can also compile interface files -> returns .cmi *)

(* open - Python's from () import () sort of keyword *)
(* module type Set =
  sig
    type elem (* abstract type definitions *)
    type t
    val empty: t
    val add: elem -> t -> t
    val member: elem -> t -> bool
end

module IntListSet: Set with type elem = int = (* adding Set = makes the internals invisible; tell OCaml what type elem is *)
  struct
    type elem = int
    type t = int list
    let empty = []
    let add i s = i :: s
    let member i s = //TODO 
  end

IntListSet.empty() *)

(* Polymorphic set type *)
(* module type PolySet =
  sig
    type 'a t
    val empty: 'a t
    val add: 'a -> 'a t -> 'a t
    val member: 'a -> 'a t -> bool
  end

module IntListSet : PolySet =
  struct
    type t = int
    let empty = []
    let add i s = i :: s
    let member i s = true
  end *)


(* Functors *)
(* OCaml's way of expressing the notion of "set whose elements have to be orderable" *)
(* It's a function on modules *)

(* OrderedSet *)
(* module type ORDERED_TYPE =
  sig
    type t
    val compare: t -> t -> int
  end

module type SET =
  sig
    type elem
    (* MORE IMPLEMENTATION *)
  end

module MakeOrderedSet(Elt: ORDERED_TYPE) : (* Functor names should start with Make *)
    (SET with type elem = Elt.t) =
  struct
    (* implementations *)
  end *)

(*
   module FS = MakeOrderedSet (struct type t = float let compare = Stdlib.compare end)
*)

(* with functors, we can take on datatype and simply make a bigger data type by adding
   more functions to it 
*)

