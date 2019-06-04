type ('result, 'action, 'model, 'state) t =  {
    apply_action : 'action -> 'state -> schedule_action:('action -> unit) -> 'model;
    result: 'result
} [@@deriving fields]

let create ~result ~apply_action = {result; apply_action}
