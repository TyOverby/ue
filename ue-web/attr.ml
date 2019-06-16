open! Core_kernel
open! Import
module V = Virtual_dom.Vdom
module J = Js_of_ocaml

type t = V.Attr.t

type string_attribute = string -> t

type event_handler = (unit -> Event.t) -> t

let id = V.Attr.id

let class_name = V.Attr.class_

let on name handler =
  let name = "on" ^ name in
  let f _dom_event =
    Event.Expert.handle (handler ()) ;
    J.Js._true
  in
  let f = J.Js.Unsafe.inject (J.Dom.handler f) in
  V.Attr.property name f

let on_click = on "click"

module Internal = struct
  let to_internal_attr = Fn.id
end
