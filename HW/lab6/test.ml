type 'a contents = Unevaluated of (unit -> 'a) | Evaluated of 'a
type 'a lazy_t = 'a contents ref

let make_lazy e = ref (Unevaluated e)
let force lz =
  match !lz with
  | Unevaluated e -> e ()
  | Evaluated e -> e