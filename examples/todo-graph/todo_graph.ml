open! Ue_web
open! Import
module Todo = Todo
module Dsl = Dsl
module List_component = List_component
module Sort = Sort

module Main_component = struct
  type model = { todos : Todo.t String.Map.t } [@@deriving fields]
  type action = Nothing.t
  type result = Vdom.Node.t

  let apply_action ~schedule_action:_ _model = Nothing.unreachable_code

  let compute ~inject:_ _model =
    Vdom.Node.div
      ~attrs:[| Vdom.Attr.class_name "title" |]
      [| Vdom.Node.text "Todo (list)" |]
  ;;

  let initial_model = { todos = Example.out }
end

let main_component = Component.of_module (module Main_component)
let list_component = List_component.list_component

let component =
  let open Component.Same_model.Let_syntax in
  let%map main_component = main_component
  and list_component =
    Component.of_subcomponent
      ~field:Main_component.Fields_of_model.todos
      List_component.list_component
  in
  let list_result =
    Vdom.Node.div
      ~attrs:[| Vdom.Attr.class_name "list-container" |]
      (Map.data list_component |> Array.of_list)
  in
  Vdom.Node.div
    ~attrs:[| Vdom.Attr.class_name "wrapper" |]
    [| main_component; list_result |]
;;

;;
Start.start Main_component.initial_model component
