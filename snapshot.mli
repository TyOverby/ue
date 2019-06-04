type ('result, 'action, 'model, 'state) t

val apply_action: ('result, 'action, 'model, 'state) t -> 'action -> 'state -> schedule_action:('action -> unit) -> 'model
val result: ('result, 'action, 'model, 'state) t -> 'result

val create 
    :  result:'result
    -> apply_action: ('action -> 'state -> schedule_action:('action -> unit) -> 'model)
    -> ('result, 'action, 'model, 'state)t
