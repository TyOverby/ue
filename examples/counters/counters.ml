open! Core_kernel
open! Ue 
open! Ue_web

module Add_counter_component = struct 
  type result = Vdom.Node.t
  type model = int Map.M(Int).t 
  type action = Add_another_counter 
 
  let apply_action ~schedule_action:_ model = function
      | Add_another_counter -> 
        let key = Map.length model in 
        Map.add_exn model ~key ~data:0

  let view ~inject _ = 
      let on_click = Vdom.Attr.on_click (fun () -> inject Add_another_counter) in
      Vdom.Node.button ~attrs:[| on_click |] [| Vdom.Node.text "Add Another Counter" |]
end

module Counter_component = struct 
  type result = Vdom.Node.t
  type model = int
  type action = Increment | Decrement

  let apply_action ~schedule_action:_ model = function 
    | Increment -> model + 1
    | Decrement -> model - 1

  let view ~inject model = 
    let button_generator string action = 
      let on_click = Vdom.Attr.on_click (fun () -> inject action) in
      Vdom.Node.button ~attrs:[| on_click |] [| Vdom.Node.text string |] 
    in 
    Vdom.Node.div 
      [| button_generator "-1" Decrement
       ; Vdom.Node.text (string_of_int model)
       ; button_generator "+1" Increment
      |]
end
;;

let counters_component = 
  let open Component in 
  of_leaf (module Counter_component) 
  |> Combinator.assoc ~comparator:(module Int)
  |> map ~f:(fun result_map -> 
    Vdom.Node.div (List.to_array (Map.data result_map)))
;;

let add_counter_component = 
  Component.of_leaf (module Add_counter_component)

let application_component = 
  let open Component.Same_model.Let_syntax in 
  let%map counter_component = counters_component 
  and add_counter_component = add_counter_component in 
  Vdom.Node.div [| add_counter_component; counter_component |]

let initial_model = Map.empty (module Int)
;;

Start.start initial_model application_component

