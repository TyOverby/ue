open! Core_kernel

type ('result, 'action, 'model) t 

val of_constant: 'result -> ('result, Nothing.t, _) t

val of_model_map
  :  f:('model -> 'result) 
  -> ('result, Nothing.t, 'model) t

val of_incr_model_map
  :  f:('model Incr.t -> 'result Incr.t) 
  -> ('result, Nothing.t, 'model) t

val map
  :  ('r1, 'action, 'model) t 
  -> f:('r1 -> 'r2)
  -> ('r2, 'action, 'model) t

module Leaf_component: sig
  module type S = sig 
    type model
    type action
    type result
    val apply_action: schedule_action:(action -> unit) -> model -> action -> model
    val view: inject:(action -> Event.t) -> model -> result
  end

  type ('result, 'action, 'model) t = 
      (module S
       with type model = 'model 
       and type action = 'action 
       and type result = 'result)
end

val of_leaf : ('result, 'action, 'model) Leaf_component.t -> ('result, 'action, 'model) t
val build_map
  :  ('result, 'action, 'model) t 
  -> comparator:('k, 'cmp) Map.comparator 
  -> (('k, 'result, 'cmp) Map.t, 'k * 'action, ('k, 'model, 'cmp) Map.t) t

module Different_model : sig 
  module Let_syntax : sig 
    val return: 'result -> ('result, Nothing.t, _) t

    val both
      :  ('r1, 'a1, 'm1) t 
      -> ('r2, 'a2, 'm2) t
      -> ('r1 * 'r2, ('a1, 'a2) Either.t, 'm1 * 'm2) t

    val map
      :  ('r1, 'action, 'model) t 
      -> f:('r1 -> 'r2)
      -> ('r2, 'action, 'model) t
  end

  val compose
    :  ('r1, 'a1, 'm1) t 
    -> ('r2, 'a2, 'm2) t
    -> ('r1 * 'r2, ('a1, 'a2) Either.t, 'm1 * 'm2) t
end

module Same_model : sig
  module Let_syntax : sig 
    val return: 'result -> ('result, Nothing.t, _) t

    val both 
      :  ('r1, 'a1, 'm) t 
      -> ('r2, 'a2, 'm) t
      -> ('r1 * 'r2, ('a1, 'a2) Either.t, 'm) t

    val map
      :  ('r1, 'action, 'model) t 
      -> f:('r1 -> 'r2)
      -> ('r2, 'action, 'model) t
  end

  val compose
    :  ('r1, 'a1, 'm) t 
    -> ('r2, 'a2, 'm) t
    -> ('r1 * 'r2, ('a1, 'a2) Either.t, 'm) t
end

module Expert : sig
  val of_full
    :  f:( old_model: 'model option Incr.t
          -> model: 'model Incr.t
          -> inject: ('action -> Event.t)
          -> ('result, 'action, 'model) Snapshot.t Incr.t) 
    -> ('result, 'action, 'model) t

  val eval 
    :  old_model:'model option Incr.t 
    -> model:'model Incr.t 
    -> inject:('action -> Event.t) 
    -> ('result, 'action, 'model) t 
    -> ('result, 'action, 'model) Snapshot.t Incr.t 
end
