open! Import
open Dsl

let x = task "hello" @> task "there" @> task "world"
let out = List.fold (Dsl.tasks ()) ~init:String.Map.empty ~f:Todo.add_to_map

;;
Map.iter_keys out ~f:print_endline
