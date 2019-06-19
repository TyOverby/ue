open! Core_kernel

(** The Component is the building-block of a Ue program. A Component can be
    thought of as a function from 'model to 'result, but (typically) through
    interaction with the 'result, 'actions can be issued which can modify
    the 'model. The typical way that a Ue program is built is by factoring
    small pieces of an application into Components, and then combining them
    with the Combinators, or Let_syntax, forming bigger and bigger
    Components. At the end, one large Component makes up the whole
    applicaiton. *)

type ('result, 'action, 'model) t

val constant : 'result -> ('result, Nothing.t, _) t
(** Returns a component with no action or model, only a constant result. *)

val of_arrow : f:('model -> 'result) -> ('result, Nothing.t, 'model) t
(** Returns a component with no action, and where the result is computed by
    applying [f] to the model. *)

val of_subcomponent :
     field:('outer_model, 'inner_model) Field.t
  -> f:('outer_model -> ('result, 'action, 'inner_model) t)
  -> ('result, 'action, 'outer_model) t
(** Creates components whose constructors require parameters from an outer
    component, and whose model is a field of that outer components model.

    Just read the type signature over and over until you get it. This one is
    really hard to write doc comments for. *)

val of_functions :
     apply_action:(   schedule_action:('action -> unit)
                   -> 'model
                   -> 'action
                   -> 'model)
  -> compute:(inject:('action -> Event.t) -> 'model -> 'result)
  -> ('result, 'action, 'model) t
(** Creates a component directly from the callbacks that are used in the
    component lifecycle. *)

val map :
  ('r1, 'action, 'model) t -> f:('r1 -> 'r2) -> ('r2, 'action, 'model) t
(** Transforms the result of a component by applying some mapping function
    [f]. *)

module Module_component : sig
  (** Many modules have the same shape, they declare the model, action, and
      result of the component, and then deinfe apply_action and view over
      those types.

      Module_component.S is intended to be used with the
      [Component.of_module] function. *)
  module type S = sig
    (**  *)
    type model

    type action

    type result

    val apply_action :
      schedule_action:(action -> unit) -> model -> action -> model
    (** The basics of [apply_action] is a transformation from from a model
        and an action into a new model. During the transformation, the
        Component can also emit more actions via [schedule_action]. *)

    val compute : inject:(action -> Event.t) -> model -> result
    (** Computes the [result] of the component based off of the [model].

        The [inject] argument is used to transform actions into
        [Ue.Event.t]s for use in event handlers like [on_click]. *)
  end

  type ('result, 'action, 'model) t =
    (module S
       with type model = 'model
        and type action = 'action
        and type result = 'result)
end

val of_module :
     ('result, 'action, 'model) Module_component.t
  -> ('result, 'action, 'model) t
(** Creates a component from a Module_component instance. *)

module Combinator : sig
  val assoc :
       ('result, 'action, 'model) t
    -> comparator:('k, 'cmp) Map.comparator
    -> (('k, 'result, 'cmp) Map.t, 'k * 'action, ('k, 'model, 'cmp) Map.t) t
  (** Transforms a component into a new component whose model and result are
      map structures. *)
end

module Different_model : sig
  (** For composing Components whose models are different. The resulting
      Component has tupled results and models, while the action is wrapped
      in [Either.t]s *)

  module Let_syntax : sig
    module Let_syntax : sig
      val return : 'result -> ('result, Nothing.t, _) t

      val both :
           ('r1, 'a1, 'm1) t
        -> ('r2, 'a2, 'm2) t
        -> ('r1 * 'r2, ('a1, 'a2) Either.t, 'm1 * 'm2) t

      val map :
           ('r1, 'action, 'model) t
        -> f:('r1 -> 'r2)
        -> ('r2, 'action, 'model) t
    end
  end

  val compose :
       ('r1, 'a1, 'm1) t
    -> ('r2, 'a2, 'm2) t
    -> ('r1 * 'r2, ('a1, 'a2) Either.t, 'm1 * 'm2) t
end

module Same_model : sig
  (** For composing component who share the same model. The resulting
      component has a tupled result, and the action is wrapped in
      [Either.t]s. The model remains the same (because it is being shared) *)

  module Let_syntax : sig
    module Let_syntax : sig
      val return : 'result -> ('result, Nothing.t, _) t

      val both :
           ('r1, 'a1, 'm) t
        -> ('r2, 'a2, 'm) t
        -> ('r1 * 'r2, ('a1, 'a2) Either.t, 'm) t

      val map :
           ('r1, 'action, 'model) t
        -> f:('r1 -> 'r2)
        -> ('r2, 'action, 'model) t
    end
  end

  val compose :
       ('r1, 'a1, 'm) t
    -> ('r2, 'a2, 'm) t
    -> ('r1 * 'r2, ('a1, 'a2) Either.t, 'm) t
end

module Incremental : sig
  val of_arrow :
    f:('model Incr.t -> 'result Incr.t) -> ('result, Nothing.t, 'model) t
  (** Same as of_arrow, but allows the user to optimize using Incremental. *)

  val of_subcomponent :
       field:('outer_model, 'inner_model) Field.t
    -> f:('outer_model Incr.t -> ('result, 'action, 'inner_model) t)
    -> ('result, 'action, 'outer_model) t

  val of_functions :
       apply_action:(   'model Incr.t
                     -> (   schedule_action:('action -> unit)
                         -> 'action
                         -> 'model)
                        Incr.t)
    -> compute:(   inject:('action -> Event.t)
                -> 'model Incr.t
                -> 'result Incr.t)
    -> ('result, 'action, 'model) t
end

module Expert : sig
  val of_full :
       f:(   old_model:'model option Incr.t
          -> model:'model Incr.t
          -> inject:('action -> Event.t)
          -> ('result, 'action, 'model) Snapshot.t Incr.t)
    -> ('result, 'action, 'model) t
  (** AH YES. The full power of the Incr_dom "Component" system.

      Gaze ye not into the abyss lest ye be known as an abyss domain expert. *)

  val eval :
       old_model:'model option Incr.t
    -> model:'model Incr.t
    -> inject:('action -> Event.t)
    -> ('result, 'action, 'model) t
    -> ('result, 'action, 'model) Snapshot.t Incr.t
  (** Do you like GADT's? I do. That's why this function is called [eval],
      and not called something that is more informative. Gotta keep those
      traditions alive somehow. *)
end
