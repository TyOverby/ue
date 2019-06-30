open! Core_kernel
open! Import
open Ue
open Ue_web

module Model = struct
  type 'm t =
    { inner: 'm
    ; cursor: Spacetime_tree.Cursor.t
    ; history: 'm Spacetime_tree.t }
  [@@deriving fields]
end

module Action = struct
  type 'a t = Inner of 'a | Set_cursor of Spacetime_tree.Cursor.t
end

module Result = struct
  type t = Ue_web.Vdom.Node.t -> Ue_web.Vdom.Node.t
end

let draw_history ~global_cursor ~inject ~data:_ ~cursor ~x:_ ~y:_ ~children
    =
  let on_click =
    Vdom.Attr.on_click (fun () -> inject (Action.Set_cursor cursor))
  in
  let button_attrs =
    if Spacetime_tree.Cursor.equal global_cursor cursor then
      [|on_click; Vdom.Attr.class_name "current"|]
    else [|on_click|]
  in
  let my_node =
    Vdom.Node.button ~attrs:button_attrs [|Vdom.Node.text "●"|]
  in
  let children =
    Vdom.Node.div
      ~attrs:[|Vdom.Attr.class_name "ch"|]
      (List.to_array children)
  in
  Vdom.Node.div ~attrs:[|Vdom.Attr.class_name "cnt"|] [|my_node; children|]

let view cursor history ~inject =
  let open Ue.Incr.Let_syntax in
  let%map cursor = cursor and history = history in
  let spacetime =
    Spacetime_tree.traverse history
      ~f:(draw_history ~inject ~global_cursor:cursor)
  in
  let spacetime =
    Vdom.Node.div
      ~attrs:[|Vdom.Attr.class_name "history_wrapper"|]
      [|spacetime|]
  in
  fun window ->
    Vdom.Node.div
      ~attrs:[|Vdom.Attr.class_name "history_wrapper_wrapper"|]
      [|window; spacetime|]

let wrap_model m =
  let history, cursor = Spacetime_tree.create m in
  {Model.inner= m; cursor; history}

let create (type r a m) (inner_component : (r, a, m) Ue.Component.t) :
    (r * Result.t, a Action.t, m Model.t) Ue.Component.t =
  let open Ue.Incr.Let_syntax in
  Ue.Component.Expert.of_full
    ~f:(fun ~old_model ~(model : m Model.t Incr.t) ~inject ->
      let inject_inner a = inject (Action.Inner a) in
      let inner_model = Incr.map model ~f:Model.inner in
      let inner_old_model =
        Incr.map old_model ~f:(Option.map ~f:Model.inner)
      in
      let inner =
        Ue.Component.Expert.eval ~old_model:inner_old_model
          ~model:inner_model ~inject:inject_inner inner_component
      in
      let apply_action =
        let%map model = model and inner = inner in
        fun ~schedule_action ->
          let schedule_action a = schedule_action (Action.Inner a) in
          function
          | Action.Inner a ->
              let inner : m =
                Ue.Snapshot.apply_action inner ~schedule_action a
              in
              let history, cursor =
                Spacetime_tree.append model.history model.cursor inner
              in
              {Model.inner; history; cursor}
          | Action.Set_cursor cursor ->
              print_endline "finding" ;
              let inner = Spacetime_tree.find model.history cursor in
              {Model.inner; history= model.history; cursor}
      in
      let result =
        let%map inner = inner
        and view =
          view ~inject (model >>| Model.cursor) (model >>| Model.history)
        in
        (Snapshot.result inner, view)
      in
      let%map apply_action = apply_action and result = result in
      let apply_action ~schedule_action a =
        print_endline "applying action" ;
        apply_action ~schedule_action a
      in
      Snapshot.create ~result ~apply_action)
