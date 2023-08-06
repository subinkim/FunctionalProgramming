(* CS 4 2022-23, Prof. Vanier
  * Lecture 11 (2/6/2023)
  * Live Coding
*)

(* Objects and Classes *)

(* e.g. Lengths *)
type length = {
  magnitude: float;
  unit_type: string;
  base: unit -> length;
  add: length -> length;
}

let rec make_meter x = 
  let base_method () = make_meter x in
  let add_method other = 
    make_meter (x +. (other.base ()).magnitude) 
  in
    { 
      magnitude = x; 
      unit_type = "meter"; 
      base = base_method;
      add = add_method
    }

let make_foot x = 
  let as_meter = 0.3048 *. x in
  let base_method () = make_meter as_meter in
  let add_method other = 
    make_meter (as_meter +. (other.base ()).magnitude) 
  in
    { 
      magnitude = x; 
      unit_type = "foot"; 
      base = base_method;
      add = add_method
    }

(* Object definition of make_meter - has very complicated types *)
(* .. = details OCaml doesn't care about *)
let rec make_meter x =
  object
    method magnitude = x
    method unit_typ = "meter"
    method base = make_meter x (* no arguments *)
    method add other = (* other can be any object that supports base method - flexibility *)
      make_meter (x +. (other#base)#magnitude) (* # = calling a method *)
  end
(* Objects in OCaml are really just extensible records *)
(* technical name = row polymorphism *)

(* Using Classes  *)
type length = 
  < 
    magnitude : float;
    unit_type : string;
    base      : length;
    add : length -> length
  >
(* this is an OBJECT type *)

(* class definition of meter *)
class meter x =
  object (self)
    method magnitude = x
    method unit_type = "meter"
    method base = new meter x
    method add (other : length) =
      new meter (x +. other#base#magnitude)
  end

let m1 = new meter 1.0;;
m1#magnitude;;

class foot x = 
  let as_meter = 0.3048 *. x in
    object
      method magnitude = x
      method unit_type = "foot"
      method base = new meter as_meter
      method add (other : length) =
        new meter (as_meter +. other#base#magnitude)
    end

(* improving meter *)
class meter x =
  object (self)
    method magnitude = x
    method unit_type = "meter"
    method base = self
    method add (other : length) =
      new meter (x +. other#base#magnitude)
  end

(* Replacing Length with a Class *)
(* use virtual keyword - abstract base class *)
class virtual length =
  object
    method virtual magnitude : float
    method virtual unit_type : string
    method virtual base      : length
    method virtual add : length -> length
  end

(* Inheritance *)
class meter x =
  object (self)
    inherit length
    method magnitude = x
    method unit_type = "meter"
    method base = (self :> length) (* :> coerce one type (meter) to another type (length) *)
    method add (other : length) =
      new meter (x +. other#base#magnitude)
  end


class virtual length =
  object
    method virtual magnitude : float
    method virtual unit_type : string
    method virtual base      : meter
    method virtual add : length -> length
  end
and meter x = (* mutually recursive case *)
  object (self)
    inherit length
    method magnitude = x
    method unit_type = "meter"
    method base = (self :> meter) (* you still have to coerce self to meter, even though it is already meter type *)
    method add (other : length) =
      new meter (x +. other#base#magnitude)
  end