type t =
  { name : string
  ; description : string option
  ; priority : string option
  ; group : string option
  ; mutable dependencies : t list
  }
[@@deriving fields]

val task : ?description:string -> ?priority:string -> ?group:string -> string -> t
val tasks : unit -> t list
val ( @> ) : t -> t -> t
