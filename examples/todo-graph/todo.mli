open! Import

type t =
  { id : string
  ; name : string
  ; description : string option
  ; priority : string option
  ; group : string option
  ; dependencies : String.Set.t
  }
[@@deriving fields]

val add_to_map : t String.Map.t -> Dsl.t -> t String.Map.t
