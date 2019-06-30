(** A Snapshot represents the state of a component at an instant in time. It
    contains inside of it a fully-computed result, as well as an
    apply_action callback which has closed over the model which constructed
    it. *)

type ('result, 'action, 'model) t

(** Extracts the apply_action callback from the Snapshot. *)
val apply_action
  :  ('result, 'action, 'model) t
  -> schedule_action:('action -> unit)
  -> 'action
  -> 'model

(** Extracts the result from the Snapshot. *)
val result : ('result, 'action, 'model) t -> 'result

val create
  :  result:'result
  -> apply_action:(schedule_action:('action -> unit) -> 'action -> 'model)
  -> ('result, 'action, 'model) t
