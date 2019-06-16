type ('result, 'action, 'model) t

val apply_action :
     ('result, 'action, 'model) t
  -> schedule_action:('action -> unit)
  -> 'action
  -> 'model

val result : ('result, 'action, 'model) t -> 'result

val create :
     result:'result
  -> apply_action:(schedule_action:('action -> unit) -> 'action -> 'model)
  -> ('result, 'action, 'model) t
