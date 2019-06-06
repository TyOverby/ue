type ('result, 'action, 'model) t =  {
    apply_action : 'action -> schedule_action:('action -> unit) -> 'model;
    result: 'result
} [@@deriving fields]

let create ~result ~apply_action = {result; apply_action}
