open! Ue 
open! Ue_web

module Counter_component = struct 
  type result = Vdom.Node.t
  type model = int
  type action = Increment | Decrement

  let apply_action ~schedule_action:_ model = function 
    | Increment -> model + 1
    | Decrement -> model - 1

  let view model ~inject = 
    let button_generator string action = 
      let on_click = Vdom.Attr.on_click (fun () -> inject action) in
      Vdom.Node.button ~attrs:[| on_click |] [| Vdom.Node.text string |] 
    in 
    Vdom.Node.div 
      [| button_generator "+1" Increment
       ; Vdom.Node.text (string_of_int model)
       ; button_generator "-1" Decrement
      |]
end
;;


let counter_component = 
  Component.of_leaf_component (module Counter_component);;

Start.start 0 counter_component

