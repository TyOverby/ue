open! Import

type t =
  { name: string
  ; description: string option
  ; priority: string option
  ; group: string option
  ; mutable dependencies: t list }
[@@deriving fields]

let tasks = ref []

let task ?description ?priority ?group name =
  let dependencies = [] in
  let t = {name; description; priority; group; dependencies} in
  tasks := t :: !tasks ;
  t

let tasks () = !tasks

let ( @> ) a b =
  a.dependencies <- b :: a.dependencies ;
  b
