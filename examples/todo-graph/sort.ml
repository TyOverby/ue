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
    type priority_list =  Sort.t list [@@deriving compare, sexp]
    type ('a, 'cmp) t = {
        priority: priority_list;
        base: 'a;
        base_comparator: ('a, 'cmp) Comparator.t
    }

    let compare a b = 
        match compare_priority_list a.priority b.priority with 
        | 0 -> a.base_comparator.compare a.base b.base 
        | other -> other

    let sexp_of_t t = 
        let base_sexped = t.base_comparator.sexp_of_t t.base in 
        let all = List.map t.priority ~f:Sort.sexp_of_t in 
        Sexp.List (List.append all [base_sexped])

    let make base base_comparator = 
        { base; base_comparator; priority = [] }

end

let rekey input output_comparator ~f = 
  let open Incr.Let_syntax in
  let%bind input_comparator = 
      let%map input = input in 
      Map.comparator input 
  in
  let init = Map.empty output_comparator in 
  Incr.Map.unordered_fold input
    ~init
    ~add:(fun ~key ~data acc -> 
        let key = f ~key ~data in 
        Map.add_exn acc ~key ~data)
    ~remove:(fun ~key ~data acc -> 
        let key = f ~key ~data in 
        Map.remove acc key)

type ('k, 'v, 'cmp, 'cmp2) sort_type = 
    ('k, 'v, 'cmp) Map.t Incr.t
    -> (('k, 'cmp) Sorted_by.t, 'v, 'cmp2) Map.t Incr.t

let sort (type k v cmp): (k, v, cmp, _) sort_type = fun input ->
    let module Sorted_by_cmp = struct 
        module T = struct 
            type t = (k, cmp) Sorted_by.t
            let compare = Sorted_by.compare
            let sexp_of_t = Sorted_by.sexp_of_t
        end 
        include T 
        include Comparator.Make (T)
    end in

    (*
    let sorted_comparator = 
        Comparator.make 
        ~compare:Sorted_by.compare
        ~sexp_of_t:Sorted_by.sexp_of_t in
    let open Incr.Let_syntax in
    let%map input = input in
    let input_comparator = Map.comparator input in
    let empty_rekeyed = Map.empty (module Sorted_by_cmp) in 
    let new_map = Map.fold input ~init:empty_rekeyed ~f:(
      fun  ~key ~data building -> 
        let key = Sorted_by.make key input_comparator in 
        Map.add_exn building ~key ~data) 
    in 
    *)
    let input_comparator = (module Sorted_by_cmp) in 
    let key_transform ~key ~data:_ = Sorted_by.make key input_
    rekey input (module Sorted_by_cmp) ~f:(fun ~key ~data -> 
        Sorted_by.make key input_comparator)
