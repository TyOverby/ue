open! Import
open Ue_web

type model = Todo.t String.Map.t
type action = string * Nothing.t

let list_item_component = Component.of_arrow ~f:(fun todo -> 
    let open Todo in 
    let class_name = Vdom.Attr.class_name in 
    let name = Vdom.Node.div
      ~attrs: [| class_name "title" |] 
      [| Vdom.Node.span_text (name todo) |] in
    let description = match description todo with
    | Some description -> Vdom.Node.div [| Vdom.Node.span_text description |]   
    | None -> Vdom.Node.div [||] 
    in
    Vdom.Node.div 
      ~attrs:[| class_name "todo-list-item"|] 
      [| name; description |])

let list_component = 
    Component.Combinator.assoc 
      ~comparator:(module String)
      list_item_component
