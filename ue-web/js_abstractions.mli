module Queue : sig
  type 'a t 

  val create: unit -> 'a t

  val enqueue: 'a t -> 'a -> unit
  val dequeue: 'a t -> 'a 
  val is_empty: _ t -> bool
end

module Mount_point : sig 
  type t

  val create_and_mount: string -> Node.t -> t
  val update: t -> Node.t -> unit
end

module Time: sig
  val update: unit -> unit 
end

module Schedule_once : sig
  type t
  val create: unit -> t 
  val schedule: t -> f:(unit -> unit) -> unit
end
