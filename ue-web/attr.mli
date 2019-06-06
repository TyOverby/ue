open! Import

type t  = Virtual_dom.Vdom.Attr.t
type string_attribute = string -> t
type event_handler = (unit -> Event.t) -> t

val id : string_attribute
val on_click: event_handler
