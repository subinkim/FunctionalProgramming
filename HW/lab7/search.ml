(* search.ml: search strategies *)
(* Student name:                *)
(* CMS cluster login name:      *)

module type Storage =
  sig
    type 'a t
    exception Empty

    val create : unit -> 'a t
    val push : 'a -> 'a t -> unit
    val pop : 'a t -> 'a
    val is_empty : 'a t -> bool
  end

module type Domain =
  sig
    type t
    val show : t -> string
    val is_solved : t -> bool
    val compare : t -> t -> int
    val next : t -> t list
  end

module Search (S : Storage) (D : Domain) =
  struct
    module DS = Set.Make(D)

    (* Part B *)
    let search init = 
      let storage = S.create () in
      let _ = S.push [init] storage in
      let rec iter visited = 
        if S.is_empty storage then raise Not_found
        else
          let his = S.pop storage in
          let board = List.hd his in
          match DS.mem board visited with
          | true -> iter visited
          | false ->
            if D.is_solved board then his
            else
              let _ = List.iter (fun c -> S.push (c::his) storage) 
                                (D.next board) in
              iter (DS.add board visited)
      in 
      iter (DS.empty)


    let show_history hist =
      (String.concat "\n----\n\n" (List.map D.show (List.rev hist))) ^ "\n"
  end

