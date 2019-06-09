open! Ue 
open! Ue_web

module Counter_component: Component.Leaf_component.S = struct 
  type result = Vdom.Node.t
  type model = int
  type action = Increment | Decrement

  let apply_action ~schedule_action:_ model = function 
    | Increment -> model + 1
    | Decrement -> model - 1

  let view model ~inject = 
    let open Vdom.Node in
    let open Vdom.Attr in

    let button_generator string action = 
      let on_click = on_click (fun () -> inject action) in
      button ~attrs:[| on_click |] [| text string |] in 

    div [| button_generator "+1" Increment
         ; Vdom.Node.text (string_of_int model)
         ; button_generator "-1" Decrement
        |]

end
;;

Start.start 0 (Component.of_leaf_component (module Counter_component))

