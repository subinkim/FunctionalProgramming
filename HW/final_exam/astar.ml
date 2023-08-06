open Pqueue

module type Task =
  sig
    type t

    val compare : t -> t -> int
    val eval    : t -> t -> int
    val next    : t -> t list
    val display : t -> string
  end

module AStar (T : Task) =
  struct
    (* Raised when no more task states are left on the queue. *)
    exception No_solution

    (* Raised when a solution is encountered while searching. *)
    exception Solved of T.t list

    (* The state of a search stored in the priority queue *)
    type qstate = {
      tstate : T.t;  (* a task state *)

      (* the list of task states that preceded this one in the search *)
      history : T.t list;  

      (* the number of moves to get to this state; this is just the length
         of the history list *)
      nmoves : int;   

      (* the overall fitness: a function of the tstate evaluation and nmoves *)
      fitness : int    
    }

    (* Make the priority queue.  Compare queue states by fitness. *)
    module Tqueue = MakePriorityQueue(struct
        type t = qstate
        let compare s1 s2 = Stdlib.compare s1.fitness s2.fitness
      end)

    (* A map of the best quality scores for each evaluated tstate. *)
    module Tmap = Map.Make(T)

    (* The state of the solver as a whole. *)
    type t = {
      queue : Tqueue.t;
      best  : int Tmap.t
    }

    (* The solver function. *)
    let solve goal init =
      let init_eval  = T.eval goal init in
      let init_state =
        {
           tstate  = init;
           history = [];
           nmoves  = 0;
           fitness = init_eval;
        }
      in
      let init_queue  = Tqueue.insert (Tqueue.empty) init_state in
      let init_best   = Tmap.empty in
      let init_solver = { queue = init_queue; best = init_best } in

       let rec next next_states state queue =
        match next_states with
        | h :: t -> 
          let score = T.eval goal h in
          if score = 0 then
            raise (Solved (init :: List.rev (h :: state.history)))
          else
            next t state
              (Tqueue.insert queue 
                { tstate = h; 
                  history = h :: state.history; 
                  nmoves = state.nmoves + 1; 
                  fitness = score + state.nmoves + 1 }) 
        | [] -> queue
      in
        
      let rec iter { queue; best }  =
        let (state, new_queue) = Tqueue.pop_min queue in
        if Tmap.mem state.tstate best && 
           Tmap.find state.tstate best <= state.nmoves then
            iter {queue = new_queue; best}
        else 
            iter {queue = next (T.next state.tstate) state new_queue; 
                  best = Tmap.add state.tstate state.nmoves best}
      in

        (* The main part of the function starts here. *)
        if init_eval = 0 then
          [init]  (* handle the case when the initial state is solved. *)
        else
          try
            iter init_solver
          with 
            | Tqueue.Empty -> raise No_solution
            | Solved tlist -> tlist
  end

