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

let rec add_to_map map dsl =
  if String.Map.mem map (Dsl.name dsl)
  then map
  else (
    let dependencies = List.map dsl.dependencies ~f:Dsl.name |> String.Set.of_list in
    let converted =
      { id = dsl.name
      ; name = dsl.name
      ; description = dsl.description
      ; priority = dsl.priority
      ; group = dsl.group
      ; dependencies
      }
    in
    let map = String.Map.add_exn map ~key:dsl.name ~data:converted in
    List.fold dsl.dependencies ~init:map ~f:add_to_map)
;;
