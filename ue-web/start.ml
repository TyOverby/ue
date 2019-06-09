open! Core_kernel
open! Import 
module J = Js_of_ocaml

module Queue = struct 
    open Js_of_ocaml
    type 'a t = 'a Js.js_array Js.t

    let create ()  = new%js Js.array_empty 
    let enqueue (arr: 'a t) ele = ignore (arr##push ele); ()
    let _dequeue (arr: 'a t) = 
        Js.Optdef.get 
          (arr##shift) 
          (fun () -> failwith "empty queue")
    let _is_empty (arr: 'a t) = arr##.length = 0
end


let start (type a m) (initial_model:m) (component: (Node.t, a, m) Component.t) = 
    let queue : a Queue.t = Queue.create () in

    let module Handler = struct 
        module Action = struct 
            type t = a
        end
        let handle = Queue.enqueue queue
    end in

    let module Action_definition = Event.Define(Handler) in
    
    let model_v = Incr.Var.create initial_model in
    let model = Incr.Var.watch model_v in
    let old_model_v = Incr.Var.create initial_model in
    let old_model = Incr.Var.watch old_model_v in


    let inject = Action_definition.inject in

    (* TODO: implement cutoff *)

    let app = Component.eval ~old_model ~model ~inject component in 
    let app_observer =  Incr.observe app in 
    Incr.stabilize ();

    let html = 
      Incr.Observer.value_exn app_observer 
      |> Snapshot.result
      |> Node.Internal.to_internal_node in
    let html_dom = 
      html 
      |> Virtual_dom.Vdom.Node.to_dom in

    let prev_html = ref html in
    let prev_elt = ref html_dom in

    let _perform_update () =
       let now =
         let date = new%js J.Js.date_now in
         Time_ns.Span.of_ms date##getTime |> Time_ns.of_span_since_epoch
       in
       Incr.Clock.advance_clock Incr.clock ~to_:now;
       Incr.stabilize ();

       let html = 
         Incr.Observer.value_exn app_observer 
         |> Snapshot.result
         |> Node.Internal.to_internal_node in
       let patch = Virtual_dom.Vdom.Node.Patch.create ~previous:!prev_html ~current:html in
       let elt = Virtual_dom.Vdom.Node.Patch.apply patch !prev_elt in
       Incr.Var.set old_model_v (Incr.Var.value model_v);
       prev_html := html;
       prev_elt := elt;
       failwith "unimplemented"
    in 
    ()
