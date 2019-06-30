open! Import 

module Sort : sig
  type t =
    | None of int
    | Int of int
    | Float of float
    | String of string
end

module Sort_key: Comparator.S

val sort: 
  ('k, 'v, 'cmp) Map.t Incr.t
  -> f:(string -> 'v -> Sort.t) Incr.t
  -> ordering:string list Incr.t
  -> (Sort_key.t, 'v, Sort_key.comparator_witness) Map.t Incr.t
