open! Core_kernel

module Arrow = struct
  type ('result, 'model) incremental = 'model Incr.t -> 'result Incr.t

  type ('result, 'model) non_incremental = 'model -> 'result

  let incremental ~f : ('result, 'model) incremental = f

  let non_incremental ~f : ('result, 'model) non_incremental = f
end

module Full = struct
  type ('result, 'action, 'model) t =
       old_model:'model option Incr.t
    -> model:'model Incr.t
    -> inject:('action -> Event.t)
    -> ('result, 'action, 'model) Snapshot.t Incr.t
end

module Module_component = struct
  module type S = sig
    type model

    type action

    type result

    val apply_action :
      schedule_action:(action -> unit) -> model -> action -> model

    val compute : inject:(action -> Event.t) -> model -> result
  end

  type ('result, 'action, 'model) t =
    (module S
       with type model = 'model
        and type action = 'action
        and type result = 'result)
end

module Assoc = struct
  type ('result, 'action, 'model, 'k, 'cmp, 'r_by_k, 'm_by_k) t =
    { comparator: ('k, 'cmp) Map.comparator
    ; r_by_k: ('r_by_k, ('k, 'result, 'cmp) Map.t) Type_equal.t
    ; m_by_k: ('m_by_k, ('k, 'model, 'cmp) Map.t) Type_equal.t }
end

module Record = struct
  type ('result, 'action, 'model) non_incremental =
    { apply_action:
        schedule_action:('action -> unit) -> 'model -> 'action -> 'model
    ; compute: inject:('action -> Event.t) -> 'model -> 'result }

  type ('result, 'action, 'model) incremental =
    { apply_action:
           'model Incr.t
        -> (schedule_action:('action -> unit) -> 'action -> 'model) Incr.t
    ; compute:
        inject:('action -> Event.t) -> 'model Incr.t -> 'result Incr.t }
end

(* OH GOD THIS TYPE SIGNATURE *)
module Computation_types (T : T3) = struct
  open T

  type ('result, 'action, 'model) eval_type =
       old_model:'model option Incr.t
    -> model:'model Incr.t
    -> inject:('action -> Event.t)
    -> ('result, 'action, 'model) t
    -> ('result, 'action, 'model) Snapshot.t Incr.t

  type ('r1, 'r2, 'a1, 'a2, 'm) same_compose_type =
       eval1:('r1, 'a1, 'm) eval_type
    -> eval2:('r2, 'a2, 'm) eval_type
    -> old_model:'m option Incr.t
    -> model:'m Incr.t
    -> inject:(('a1, 'a2) Either.t -> Event.t)
    -> ('r1, 'a1, 'm) t
    -> ('r2, 'a2, 'm) t
    -> ('r1 * 'r2, ('a1, 'a2) Either.t, 'm) Snapshot.t Incr.t

  type ('r1, 'r2, 'a1, 'a2, 'm1, 'm2) disparate_compose_type =
       eval1:('r1, 'a1, 'm1) eval_type
    -> eval2:('r2, 'a2, 'm2) eval_type
    -> old_model:('m1 * 'm2) option Incr.t
    -> model:('m1 * 'm2) Incr.t
    -> inject:(('a1, 'a2) Either.t -> Event.t)
    -> ('r1, 'a1, 'm1) t
    -> ('r2, 'a2, 'm2) t
    -> ('r1 * 'r2, ('a1, 'a2) Either.t, 'm1 * 'm2) Snapshot.t Incr.t
end
