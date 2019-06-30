open! Core_kernel

type 'a t [@@deriving sexp]

module Cursor : sig
  type t

  val to_string : t -> string
  val equal : t -> t -> bool
end

val create : 'a -> 'a t * Cursor.t
val append : 'a t -> Cursor.t -> 'a -> 'a t * Cursor.t
val find : 'a t -> Cursor.t -> 'a
val length : 'a t -> int
val height : 'a t -> int

val traverse
  :  'a t
  -> f:(data:'a -> cursor:Cursor.t -> x:int -> y:int -> children:'r list -> 'r)
  -> 'r
