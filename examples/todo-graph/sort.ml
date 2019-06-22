open! Import

module type Id = sig
  type t [@@deriving sexp, compare]
end

module Sort_key(M: Id) = struct 
    module T = struct
        type t = 
            | None of int 
            | Int of int 
            | String of string 
            | Id of M.t
            [@@deriving sexp, compare]
    end
    include T
    include Comparable.Make(T)
end

module Sort_type(M: Id): Comparable.S = struct 
    module T = struct 
        module Key = Sort_key(M) 
        type t = Key.t list  [@@deriving sexp, compare]
    end 

    include T 
    include Comparable.Make(T)
end

module Tsx = struct 
    type t = {
        a: int;
        b: string
    } [@@deriving fields]

end

type get_sorter_function = (module : Id) -> 't -> string -> 

let () = Tsx.Fields.n
