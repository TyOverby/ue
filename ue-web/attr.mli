open! Import

type t 
type string_attribute = string -> t
type event_handler = (unit -> Event.t) -> t

val id : string_attribute
val class_name: string_attribute
val on_click: event_handler

module Internal : sig
  val to_internal_attr: t -> Virtual_dom.Vdom.Attr.t
end
