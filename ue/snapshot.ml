type ('result, 'action, 'model) t =
  { apply_action: schedule_action:('action -> unit) -> 'action -> 'model
  ; result: 'result }
[@@deriving fields]

let create ~result ~apply_action = {result; apply_action}
