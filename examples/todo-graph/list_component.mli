open! Import
open Ue_web

type model = Todo.t String.Map.t
type action

val list_component : (Vdom.Node.t String.Map.t, action, model) Component.t
