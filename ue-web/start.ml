open! Core_kernel
open! Import 

module Queue = struct 
    open Js_of_ocaml
    type 'a t = 'a Js.js_array Js.t

    let create ()  = new%js Js.array_empty 
    let enqueue (arr: 'a t) ele = ignore (arr##push ele); ()
    let dequeue (arr: 'a t) = 
        Js.Optdef.get 
          (arr##shift) 
          (fun () -> failwith "empty queue")
    let is_empty (arr: 'a t) = arr##.length = 0
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

    let _app = Component.eval ~old_model ~model ~inject component in 

    ()
