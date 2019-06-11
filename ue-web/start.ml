open! Core_kernel
open! Import 
open Js_abstractions
module J = Js_of_ocaml


let start (type a m) (initial_model:m) (component: (Node.t, a, m) Component.t) = 
    let queue : a Queue.t = Queue.create () in

    let module User_action_handler = struct 
        module Action = struct 
            type t = a
        end
        let handle = Queue.enqueue queue
    end in

    let module User_action_definition = 
        Event.Define(User_action_handler) in
    
    let model_v = Incr.Var.create initial_model in
    let model = Incr.Var.watch model_v in
    let old_model_v = Incr.Var.create initial_model in
    let old_model = Incr.Var.watch old_model_v in

    let inject = User_action_definition.inject in
    let schedule_action = Queue.enqueue queue in 

    (* TODO: implement cutoff *)

    let app = Component.eval ~old_model ~model ~inject component in 
    let app_observer =  Incr.observe app in 
    Incr.stabilize ();

    let read_app () = Incr.Observer.value_exn app_observer in

    let html = Snapshot.result (read_app ()) in
    let mount_point = Mount_point.create_and_mount "app" html in
    let scheduler = Schedule_once.create () in

    let rec apply_all_actions (apply_action: schedule_action:(a -> unit) -> a -> m) = 
        if Queue.is_empty queue 
        then Incr.stabilize ()
        else (
          let action = Queue.dequeue queue in
          let new_model = apply_action ~schedule_action action in 
          Incr.Var.set model_v new_model;
          Incr.stabilize ();
          apply_all_actions apply_action)
    in

    let rec perform_update () =
       Time.update();
       let app = read_app() in 
       let apply_action = Snapshot.apply_action app in
       apply_all_actions apply_action;

       let html = Snapshot.result (read_app ()) in
       Mount_point.update mount_point html;
       Incr.Var.set old_model_v (Incr.Var.value model_v);
       Schedule_once.schedule scheduler ~f:perform_update 
    in 

    Schedule_once.schedule scheduler ~f:perform_update;
