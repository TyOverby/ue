open! Core_kernel
open Incr.Let_syntax
open Types
module Module_component = Module_component

type 'a witnessed_action =
  { action : 'a
  ; witness : 'a Core_kernel.Type_equal.Id.t
  }

type erased_action = Erased : _ witnessed_action -> erased_action

type ('result, 'action, 'model) t =
  (* Non-incremental Constructors *)
  | Constant : 'result -> ('result, Nothing.t, _) t
  | Arrow : ('result, 'model) Arrow.non_incremental -> ('result, Nothing.t, 'model) t
  | Subcomponent :
      ('result, 'action, 'inner_model) t * ('outer_model, 'inner_model) Field.t
      -> ('result, 'action, 'outer_model) t
  | Module :
      ('result, 'action, 'model) Module_component.t
      -> ('result, 'action, 'model) t
  | Function :
      ('result, 'action, 'model) Record.non_incremental
      -> ('result, 'action, 'model) t
  | Erased_action :
      ('result, 'action, 'model) t * 'action Type_equal.Id.t
      -> ('result, erased_action, 'model) t
  (* Incremental Constructors *)
  | Arrow_incr : ('result, 'model) Arrow.incremental -> ('result, Nothing.t, 'model) t
  | Subcomponent_incr :
      ('outer_model Incr.t -> ('result, 'action, 'inner_model) t)
      * ('outer_model, 'inner_model) Field.t
      -> ('result, 'action, 'outer_model) t
  | Function_incr : ('r, 'a, 'm) Record.incremental -> ('r, 'a, 'm) t
  (* Composition and Combinators *)
  | Map : ('r1, 'a, 'm) t * ('r1 -> 'r2) -> ('r2, 'a, 'm) t
  | Compose_similar :
      ('r1, 'a1, 'model) t * ('r2, 'a2, 'model) t
      -> ('r1 * 'r2, ('a1, 'a2) Either.t, 'model) t
  | Compose_disparate :
      ('r1, 'a1, 'm1) t * ('r2, 'a2, 'm2) t
      -> ('r1 * 'r2, ('a1, 'a2) Either.t, 'm1 * 'm2) t
  | Assoc :
      ('result, 'action, 'model) t
      * ('result, 'action, 'model, 'k, 'cmp, 'r_by_k, 'm_by_k) Assoc.t
      -> ('r_by_k, 'k * 'action, 'm_by_k) t
  | Full : ('result, 'action, 'model) Full.t -> ('result, 'action, 'model) t

module C = Computation_types (struct
  type nonrec ('r, 'a, 'm) t = ('r, 'a, 'm) t
end)

open C

module Similar = struct
  let compose : (_, _, _, _, _) same_compose_type =
   fun ~eval1 ~eval2 ~old_model ~model ~inject a b ->
    let inject_a e = inject (Either.first e) in
    let inject_b e = inject (Either.second e) in
    let%map a = eval1 ~old_model ~model ~inject:inject_a a
    and b = eval2 ~old_model ~model ~inject:inject_b b in
    let apply_action ~schedule_action action =
      match action with
      | Either.First a_action ->
        let schedule_action a = schedule_action (Either.first a) in
        Snapshot.apply_action a a_action ~schedule_action
      | Either.Second b_action ->
        let schedule_action b = schedule_action (Either.second b) in
        Snapshot.apply_action b b_action ~schedule_action
    in
    let result = Snapshot.result a, Snapshot.result b in
    Snapshot.create ~result ~apply_action
 ;;
end

module Disparate = struct
  let compose : (_, _, _, _, _, _) disparate_compose_type =
   fun ~eval1 ~eval2 ~old_model ~model ~inject a b ->
    let inject_a e = inject (Either.first e) in
    let inject_b e = inject (Either.second e) in
    let model_a = Incr.map model ~f:Tuple2.get1 in
    let model_b = Incr.map model ~f:Tuple2.get2 in
    let old_model_a = Incr.map old_model ~f:(Option.map ~f:Tuple2.get1) in
    let old_model_b = Incr.map old_model ~f:(Option.map ~f:Tuple2.get2) in
    let%map a = eval1 ~old_model:old_model_a ~model:model_a ~inject:inject_a a
    and b = eval2 ~old_model:old_model_b ~model:model_b ~inject:inject_b b
    and model_a = model_a
    and model_b = model_b in
    let apply_action ~schedule_action action =
      match action with
      | Either.First a_action ->
        let schedule_action a = schedule_action (Either.first a) in
        Snapshot.apply_action a a_action ~schedule_action, model_b
      | Either.Second b_action ->
        let schedule_action b = schedule_action (Either.second b) in
        model_a, Snapshot.apply_action b b_action ~schedule_action
    in
    let result = Snapshot.result a, Snapshot.result b in
    Snapshot.create ~result ~apply_action
 ;;
end

(* GADTS ARE TRULY THE BEST OH LORD *)
let rec eval : type r a m. (r, a, m) eval_type =
 fun ~old_model ~model ~inject -> function
  | Constant result ->
    Incr.const
      (Snapshot.create ~result ~apply_action:(fun ~schedule_action:_ ->
           Nothing.unreachable_code))
  | Arrow f ->
    let%map model = model in
    Snapshot.create ~result:(f model) ~apply_action:(fun ~schedule_action:_ ->
        Nothing.unreachable_code)
  | Arrow_incr f ->
    let%map result = f model in
    Snapshot.create ~result ~apply_action:(fun ~schedule_action:_ ->
        Nothing.unreachable_code)
  | Full f -> f ~old_model ~model ~inject
  | Function r ->
    let%map model = model in
    let result = r.compute ~inject model in
    let apply_action ~schedule_action a = r.apply_action ~schedule_action model a in
    Snapshot.create ~result ~apply_action
  | Function_incr r ->
    let%map result = r.compute ~inject model
    and apply_action = r.apply_action model in
    Snapshot.create ~result ~apply_action
  | Subcomponent_incr (f, field) ->
    let component = optimize (f model) in
    let model_downwards = Incr.map model ~f:(Field.get field) in
    let old_model_downwards = Incr.map old_model ~f:(Option.map ~f:(Field.get field)) in
    let%map snapshot =
      eval component ~old_model:old_model_downwards ~model:model_downwards ~inject
    and model = model in
    let result = Snapshot.result snapshot in
    let apply_action ~schedule_action a =
      let inner_model = Snapshot.apply_action snapshot ~schedule_action a in
      Field.fset field model inner_model
    in
    Snapshot.create ~result ~apply_action
  | Subcomponent (t, field) ->
    let component = t in
    let model_downwards = Incr.map model ~f:(Field.get field) in
    let old_model_downwards = Incr.map old_model ~f:(Option.map ~f:(Field.get field)) in
    let%map snapshot =
      eval component ~old_model:old_model_downwards ~model:model_downwards ~inject
    and model = model in
    let result = Snapshot.result snapshot in
    let apply_action ~schedule_action a =
      let inner_model = Snapshot.apply_action snapshot ~schedule_action a in
      Field.fset field model inner_model
    in
    Snapshot.create ~result ~apply_action
  | Erased_action (t, witness) ->
    let snapshot_incr =
      eval t ~old_model ~model ~inject:(fun action ->
          inject (Erased { action; witness }))
    in
    let%map snapshot = snapshot_incr
    and model = model in
    let apply_action ~schedule_action (Erased a) =
      match Type_equal.Id.same_witness a.witness witness with
      | Some T ->
        Snapshot.apply_action
          snapshot
          ~schedule_action:(fun action -> schedule_action (Erased { action; witness }))
          a.action
      | None -> model
    in
    let result = Snapshot.result snapshot in
    Snapshot.create ~result ~apply_action
  | Map (t, f) ->
    let%map snapshot = eval ~old_model ~model ~inject t in
    Snapshot.create
      ~result:(f (Snapshot.result snapshot))
      ~apply_action:(Snapshot.apply_action snapshot)
  | Module module_test ->
    let module M = (val module_test : Module_component.S
                      with type result = r
                       and type action = a
                       and type model = m)
    in
    let%map model = model in
    let result = M.compute model ~inject in
    let apply_action = M.apply_action model in
    Snapshot.create ~result ~apply_action
  | Compose_similar (a, b) ->
    Similar.compose ~eval1:eval ~eval2:eval ~old_model ~model ~inject a b
  | Compose_disparate (a, b) ->
    Disparate.compose ~eval1:eval ~eval2:eval ~old_model ~model ~inject a b
  | Assoc (t, { comparator; r_by_k; m_by_k }) ->
    (* I couldn't figure out how to pull this out into another function *)
    let T = r_by_k in
    let T = m_by_k in
    let old_model =
      Incr.map old_model ~f:(function
          | Some m -> m
          | None -> Map.empty comparator)
    in
    let model_and_old_model_map =
      Incr.Map.merge model old_model ~f:(fun ~key:_ ->
        function
        | `Left model -> Some (model, None)
        | `Right _ -> None
        | `Both (model, old_model) -> Some (model, Some old_model))
    in
    let snapshot_map =
      Incr.Map.mapi' model_and_old_model_map ~f:(fun ~key ~data:model_and_old_model ->
          let model = Incr.map model_and_old_model ~f:Tuple2.get1 in
          let old_model = Incr.map model_and_old_model ~f:Tuple2.get2 in
          let inject action = inject (key, action) in
          eval ~old_model ~model ~inject t)
    in
    let results_map =
      Incr.Map.mapi snapshot_map ~f:(fun ~key:_ ~data:snapshot ->
          Snapshot.result snapshot)
    in
    let apply_action =
      let%map action_map =
        Incr.Map.mapi snapshot_map ~f:(fun ~key:_ ~data -> Snapshot.apply_action data)
      and model = model in
      fun ~schedule_action action ->
        let id, action = action in
        let schedule_action a = schedule_action (id, a) in
        match Map.find action_map id with
        | None -> model (* drop it on the floor *)
        | Some apply_action ->
          let data = apply_action ~schedule_action action in
          Map.set model ~key:id ~data
    in
    let%map apply_action = apply_action
    and result = results_map in
    Snapshot.create ~result ~apply_action

(* HEY ZANDER CHECK THIS SHIT OUT *)
and optimize : type r a m. (r, a, m) optimize_type = function
  | Map (c, f) ->
    (match optimize c with
    | Constant r -> Constant (f r)
    | Map (c, g) -> Map (c, fun a -> f (g a))
    | Arrow arrow -> Arrow (fun m -> f (arrow m))
    | Function { apply_action; compute } ->
      let compute ~inject model = f (compute ~inject model) in
      Function { apply_action; compute }
    | other -> Map (other, f))
  | Compose_similar (l, r) ->
    let l = optimize l in
    let r = optimize r in
    Compose_similar (l, r)
  | Compose_disparate (l, r) ->
    let l = optimize l in
    let r = optimize r in
    Compose_disparate (l, r)
  | Assoc (t, cmp) -> Assoc (optimize t, cmp)
  | other -> other
;;

(* Constructor Functions *)
let return r = Constant r
let constant = return
let of_arrow ~f = Arrow (Arrow.non_incremental ~f)
let of_module m = Module m
let of_functions ~apply_action ~compute = Function { apply_action; compute }
let of_subcomponent ~field t = Subcomponent (t, field)
let map t ~f = Map (t, f)

let erase_action t =
  let id =
    Type_equal.Id.create ~name:"erased action" (fun _ -> Sexp.Atom "erased action")
  in
  Erased_action (t, id)
;;

let erase_action_with t id = Erased_action (t, id)

module Combinator = struct
  let assoc t ~comparator =
    let open Core_kernel.Type_equal in
    Assoc (t, { r_by_k = T; m_by_k = T; comparator })
  ;;
end

module Different_model = struct
  let compose a b = Compose_disparate (a, b)

  module Let_syntax = struct
    module Let_syntax = struct
      let return = return
      let both = compose
      let map = map
    end
  end
end

module Same_model = struct
  let compose a b = Compose_similar (a, b)

  module Let_syntax = struct
    module Let_syntax = struct
      let return = return
      let both = compose
      let map = map
    end
  end
end

module Incremental = struct
  let of_arrow ~f = Arrow_incr (Arrow.incremental ~f)
  let of_subcomponent ~field ~f = Subcomponent_incr (f, field)
  let of_functions ~apply_action ~compute = Function_incr { apply_action; compute }
end

module Expert = struct
  let of_full ~f = Full f
  let eval = eval
  let optimize = optimize
end
