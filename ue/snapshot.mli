type ('result, 'action, 'model) t

val apply_action: ('result, 'action, 'model) t -> 'action ->  schedule_action:('action -> unit) -> 'model
val result: ('result, 'action, 'model) t -> 'result

val create 
    :  result:'result
    -> apply_action: ('action -> schedule_action:('action -> unit) -> 'model)
    -> ('result, 'action, 'model) t
