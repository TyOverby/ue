open! Import

module Sort = struct
  type t =
    | None of int
    | Int of int
    | Float of float
    | String of string
  [@@deriving sexp, compare]
end

module Sorted_by = struct
  module T = struct
    type priority_list = Sort.t list [@@deriving compare, sexp]

    type 'a t =
      { priority : priority_list
      ; typeid : 'a Type_equal.Id.t
      ; base : 'a
      ; compare : 'a -> 'a -> int
      ; sexp_of_base : 'a -> Sexp.t
      }

    let compare a b =
      match compare_priority_list a.priority b.priority with
      | 0 -> a.compare a.base b.base
      | other -> other
    ;;

    let sexp_of_t t =
      let base_sexped = Type_equal.Id.to_sexp t.typeid t.base in
      let all = List.map t.priority ~f:Sort.sexp_of_t in
      Sexp.List (List.append all [ base_sexped ])
    ;;

    let make base ~compare ~sexp_of_base ~typeid =
      { base; compare; sexp_of_base; typeid; priority = [] }
    ;;
  end

  module Cmpr = struct
    type t = Hide : _ T.t -> t

    let compare (Hide a) (Hide b) =
      let T = Core_kernel.Type_equal.Id.same_witness_exn a.typeid b.typeid in
      T.compare a b
    ;;

    let sexp_of_t (Hide a) = T.sexp_of_t a

    include (val Comparator.make ~compare ~sexp_of_t)
  end

  include T
end

module Sort_key = Sorted_by.Cmpr

let rekey input output_comparator ~f =
  let init = Map.empty output_comparator in
  Incr.Map.unordered_fold
    input
    ~init
    ~add:(fun ~key ~data acc ->
      let key = f ~key ~data in
      Map.add_exn acc ~key ~data)
    ~remove:(fun ~key ~data acc ->
      let key = f ~key ~data in
      Map.remove acc key)
;;

let sort input ~f ~ordering =
  let open Incr.Let_syntax in
  let%bind cmp = input >>| Map.comparator
  and f = f
  and ordering = ordering in
  let compare = cmp.compare in
  let sexp_of_base = cmp.sexp_of_t in
  let typeid = Type_equal.Id.create ~name:"id" sexp_of_base in
  let key_transform ~key ~data =
    let base = Sorted_by.make key ~compare ~sexp_of_base ~typeid in
    let priority = List.map ordering ~f:(Fn.flip f data) in
    Sorted_by.Cmpr.Hide { base with priority }
  in
  rekey input (module Sorted_by.Cmpr) ~f:key_transform
;;
