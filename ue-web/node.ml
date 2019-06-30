open! Core_kernel
module V = Virtual_dom.Vdom

type t = V.Node.t

type node_creator = ?key:string -> ?attrs:Attr.t Array.t -> t Array.t -> t

let verify_attributes attributes =
  Array.to_list attributes |> List.map ~f:Attr.Internal.to_internal_attr

let create_node node_name : node_creator =
 fun ?key ?attrs children ->
  let attr_list =
    match attrs with None -> [] | Some attrs -> verify_attributes attrs
  in
  let child_list = Array.to_list children in
  V.Node.create ?key node_name attr_list child_list

let div = create_node "div"

let span = create_node "span"

let button = create_node "button"

let span_text t = V.Node.span [] [V.Node.text t]

let text = V.Node.text

module Internal = struct
  let to_internal_node = Fn.id
end
