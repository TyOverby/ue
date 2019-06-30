open! Core_kernel
open! Js_of_ocaml

module Queue = struct
  type 'a t = 'a Js.js_array Js.t

  let create () = new%js Js.array_empty

  let enqueue (arr : 'a t) ele =
    ignore (arr##push ele) ;
    ()

  let dequeue (arr : 'a t) =
    Js.Optdef.get arr##shift (fun () -> failwith "empty queue")

  let is_empty (arr : 'a t) = arr##.length = 0
end

module Mount_point = struct
  type t =
    { dom_node: Dom_html.element Js.t ref
    ; last_vdom: Virtual_dom.Vdom.Node.t ref }

  let create_and_mount dom_id vdom =
    let elem = Dom_html.getElementById dom_id in
    let vdom = Node.Internal.to_internal_node vdom in
    let html_dom = Virtual_dom.Vdom.Node.to_dom vdom in
    let parent =
      elem##.parentNode |> Js.Opt.to_option |> Option.value_exn
    in
    Dom.replaceChild parent html_dom elem ;
    {dom_node= ref html_dom; last_vdom= ref vdom}

  let update t vdom =
    let vdom = Node.Internal.to_internal_node vdom in
    let patch =
      Virtual_dom.Vdom.Node.Patch.create ~previous:!(t.last_vdom)
        ~current:vdom
    in
    let elt = Virtual_dom.Vdom.Node.Patch.apply patch !(t.dom_node) in
    t.dom_node := elt ;
    t.last_vdom := vdom
end

module Time = struct
  open Import

  let update () =
    let now =
      let date = new%js Js.date_now in
      Time_ns.Span.of_ms date##getTime |> Time_ns.of_span_since_epoch
    in
    Incr.Clock.advance_clock Incr.clock ~to_:now
end

module Schedule_once = struct
  type t = bool ref

  let create () = ref false

  (* TODO: install schedule context here? *)
  let schedule t ~f =
    let actually_schedule () =
      let cb =
        Js.wrap_callback (fun _ ->
            f () ;
            t := false)
      in
      let result = Dom_html.window##requestAnimationFrame cb in
      ignore result
    in
    if !t then () else actually_schedule ()
end
