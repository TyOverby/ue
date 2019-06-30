open! Core_kernel

type 'a t = {value: 'a; children: 'a t Int.Map.t} [@@deriving sexp]

module Cursor = struct
  type t = int list

  let to_string t = t |> List.sexp_of_t Int.sexp_of_t |> Sexp.to_string

  let equal = List.equal Int.equal
end

let create init = ({value= init; children= Int.Map.empty}, [])

let rec append tree cursor to_insert =
  match cursor with
  | [] ->
      let new_leaf = {value= to_insert; children= Int.Map.empty} in
      let new_cursor_pos = Int.Map.length tree.children in
      let children =
        Int.Map.add_exn tree.children ~key:new_cursor_pos ~data:new_leaf
      in
      ({tree with children}, [new_cursor_pos])
  | x :: rest ->
      let child = Int.Map.find_exn tree.children x in
      let new_child, new_cursor = append child rest to_insert in
      let children =
        Int.Map.update tree.children x ~f:(Fn.const new_child)
      in
      ({tree with children}, x :: new_cursor)

let rec find tree cursor =
  print_s [%message (cursor : int list)] ;
  match cursor with
  | [] -> tree.value
  | x :: rest -> find (Int.Map.find_exn tree.children x) rest

let rec length tree =
  1
  + ( tree.children |> Map.data |> List.map ~f:length
    |> List.fold ~init:0 ~f:Int.max )

let rec height tree =
  match Map.length tree.children with
  | 0 -> 1
  | _ ->
      tree.children |> Map.data |> List.map ~f:height
      |> List.fold_left ~init:0 ~f:( + )

let traverse tree ~f =
  let rec traverse_impl {children; value; _} ~cursor ~f ~x ~y =
    let children = Map.data children in
    match children with
    | [] -> (f ~data:value ~cursor ~x ~y ~children:[], y)
    | all ->
        let ret, y =
          List.foldi all ~init:([], y) ~f:(fun i (res, cur_y) child ->
              let cursor = List.append cursor [i] in
              let ret, y = traverse_impl child ~cursor ~f ~x ~y:cur_y in
              (ret :: res, y))
        in
        (f ~data:value ~cursor ~x ~y ~children:ret, y)
  in
  let r, _ = traverse_impl tree ~cursor:[] ~f ~x:0 ~y:0 in
  r
