open Boardrep

module type BoardMetric =
  sig
    type t

    val distance : t -> t -> int
  end

module Hamming(B : BoardRep) : BoardMetric with type t = B.t =
  struct
    type t = B.t

    let distance b1 b2 =
      let size1 = B.get_size b1 in
      let size2 = B.get_size b2 in
      if size1 <> size2 then failwith "incompatible board sizes"
      else
        let rec iter row col score =
          match row, col with 
          | _, b when b = size1 -> iter (row + 1) 0 score
          | a, _ when a = size1 -> score
          | a, b ->
            if B.get b1 (a, b) <> B.get b2 (a, b) && B.get b2 (a, b) <> 0
              then iter row (col + 1) (score + 1)
            else iter row (col + 1) score
        in iter 0 0 0
  end

module Manhattan(B : BoardRep) : BoardMetric with type t = B.t =
  struct
    type t = B.t

    let distance b1 b2 =
      let size1 = B.get_size b1 in
      let size2 = B.get_size b2 in
      if size1 <> size2 then failwith "incompatible board sizes"
      else
        let locs b size =
          let rec iter row col arr = 
            match row, col with 
            | _, y when y = size -> iter (row + 1) 0 arr
            | x, _ when x = size -> arr
            | _, _ ->
              arr.(B.get b (row, col)) <- (row, col);
              iter row (col + 1) arr
          in iter 0 0 (Array.make (size * size) (0, 0))
        in
        let arr1 = locs b1 size1 in
        let arr2 = locs b2 size2 in
        let rec calc_score i score =
          if i < size1 * size1 then
            let (x1, y1) = Array.get arr1 i in
            let (x2, y2) = Array.get arr2 i in
            calc_score (i + 1) (score + (abs(x1 - x2) + abs(y1 - y2)))
          else score
        in calc_score 1 0
  end