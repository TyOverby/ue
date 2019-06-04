module type Handler = sig
  module Action : sig
    type t
  end

  val handle : Action.t -> unit
end

module type S = sig
  type action
  type t = private ..
  type t += C : action -> t

  val inject : action -> t
end

module type Event = sig
  type t = private ..

  type t +=
    | Nop
    | All of t list

  module type Handler = Handler
  module type S = S

  module Define (Handler : Handler) :
    S with type action := Handler.Action.t and type t := t

  module Expert : sig
    val handle : t -> unit
  end
end
