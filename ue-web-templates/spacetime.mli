open! Core_kernel
open! Import 
module Model : sig 
    type 'm t
end 
module Action : sig
    type 'a t
end
module Result : sig 
    type t = Ue_web.Vdom.Node.t -> Ue_web.Vdom.Node.t
end

val wrap_model: 'm -> 'm Model.t
val create: ('r, 'a, 'm) Ue.Component.t -> ('r * Result.t, 'a Action.t, 'm Model.t) Ue.Component.t
