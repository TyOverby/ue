open! Core_kernel

open Incr.Let_syntax

module Dep = struct 
    type ('result, 'model) t = 'model Incr.t -> 'result Incr.t 

    let of_fun ~f = f
    let of_mapped ~f = Incr.map ~f
end

module Full = struct
    type ('result, 'action, 'model) t 
        =  old_model: 'model option Incr.t
        -> model: 'model Incr.t
        -> inject: ('action -> Event.t)
        -> ('result, 'action, 'model) Snapshot.t Incr.t
end


module Leaf_component = struct
  module type S = sig 
    type model
    type action
    type result
    val apply_action: schedule_action:(action -> unit) -> model -> action -> model
    val view: inject:(action -> Event.t) -> model -> result
  end

  type ('result, 'action, 'model) t = 
      (module S
       with type model = 'model 
       and type action = 'action 
       and type result = 'result)
end

type ('result, 'action, 'model) t =
    | Constant : 'result -> ('result, Nothing.t, _) t
    | Dep :  ('result, 'model) Dep.t -> ('result, Nothing.t, 'model) t
    | Full : ('result, 'action, 'model) Full.t -> ('result, 'action, 'model) t
    | Map : ('r1, 'a, 'm) t * ('r1 -> 'r2) -> ('r2, 'a, 'm) t 
    | Leaf: ('result, 'action, 'model) Leaf_component.t -> ('result, 'action, 'model) t
    | Dict: 
        { t: ('result, 'action, 'model) t 
        ; comparator: ('k, 'cmp) Map.comparator
        ; r_by_k : ('r_by_k, ('k, 'result, 'cmp) Map.t) Type_equal.t
        ; m_by_k : ('m_by_k, ('k, 'model, 'cmp) Map.t) Type_equal.t
        }
      -> ('r_by_k, 'k * 'action, 'm_by_k) t 
    | Compose_similar : 
         ('r1, 'a1, 'model) t 
       * ('r2, 'a2, 'model) t 
      -> ('r1 * 'r2, ('a1, 'a2) Either.t, 'model) t 
    | Compose_disparate :  
         ('r1, 'a1, 'm1) t 
       * ('r2, 'a2, 'm2) t
      -> ('r1 * 'r2, ('a1, 'a2) Either.t, 'm1 * 'm2) t

(* OH GOD THIS TYPE SIGNATURE *)
type ('result, 'action, 'model) eval_type 
  =  old_model:'model option Incr.t 
  -> model:'model Incr.t 
  -> inject:('action -> Event.t) 
  -> ('result, 'action, 'model) t 
  -> ('result, 'action, 'model) Snapshot.t Incr.t 

module Similar = struct 
    type ('r1, 'r2, 'a1, 'a2, 'm) compose_type 
      =  eval1:('r1, 'a1, 'm) eval_type 
      -> eval2:('r2, 'a2, 'm) eval_type 
      -> old_model:'m option Incr.t
      -> model:'m Incr.t
      -> inject:(('a1, 'a2) Either.t -> Event.t)
      -> ('r1, 'a1, 'm) t 
      -> ('r2, 'a2, 'm) t 
      -> ('r1 * 'r2, ('a1, 'a2) Either.t, 'm) Snapshot.t Incr.t

    let compose: (_, _, _, _, _) compose_type = fun ~eval1 ~eval2 ~old_model ~model ~inject a b ->
      let inject_a e = inject (Either.first e) in
      let inject_b e = inject (Either.second e) in
      let%map a = eval1 ~old_model ~model ~inject:inject_a a
      and     b = eval2 ~old_model ~model ~inject:inject_b b in
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
end

module Disparate = struct 
    type ('r1, 'r2, 'a1, 'a2, 'm1, 'm2) compose_type 
      =  eval1:('r1, 'a1, 'm1) eval_type 
      -> eval2:('r2, 'a2, 'm2) eval_type 
      -> old_model:('m1 * 'm2) option Incr.t
      -> model:('m1 * 'm2) Incr.t
      -> inject:(('a1, 'a2) Either.t -> Event.t)
      -> ('r1, 'a1, 'm1) t 
      -> ('r2, 'a2, 'm2) t 
      -> ('r1 * 'r2, ('a1, 'a2) Either.t, 'm1 * 'm2) Snapshot.t Incr.t

    let compose: (_, _, _, _, _, _) compose_type = fun ~eval1 ~eval2 ~old_model ~model ~inject a b ->
      let inject_a e = inject (Either.first e) in
      let inject_b e = inject (Either.second e) in
      let model_a = Incr.map model ~f:(Tuple2.get1) in 
      let model_b = Incr.map model ~f:(Tuple2.get2) in 
      let old_model_a = Incr.map old_model ~f:(Option.map ~f:Tuple2.get1) in 
      let old_model_b = Incr.map old_model ~f:(Option.map ~f:Tuple2.get2) in 

      let%map   a = eval1 ~old_model:old_model_a ~model:model_a ~inject:inject_a a
      and       b = eval2 ~old_model:old_model_b ~model:model_b ~inject:inject_b b 
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
end

(* GADTS ARE TRULY THE BEST OH LORD *)
let rec eval: type r a m . (r, a, m) eval_type = 
    fun ~old_model ~model ~inject -> function 
    | Constant result -> Incr.const (Snapshot.create ~result ~apply_action:(fun ~schedule_action:_ ->  Nothing.unreachable_code))
    | Dep f -> 
      let%map result = f model in
      Snapshot.create ~result ~apply_action:(fun ~schedule_action:_ -> Nothing.unreachable_code)
    | Full f -> f ~old_model ~model ~inject
    | Map (t, f) -> 
      let%map snapshot = eval ~old_model ~model ~inject t in
      Snapshot.create 
        ~result:(f (Snapshot.result snapshot)) 
        ~apply_action:(Snapshot.apply_action snapshot)
    | Leaf module_test -> 
      let module M = (val module_test: Leaf_component.S
                      with type result = r 
                      and  type action = a 
                      and  type model  = m) in
      let%map model = model in
      let result = M.view model ~inject in
      let apply_action = M.apply_action model in 
      Snapshot.create ~result ~apply_action
    | Compose_similar (a, b) -> Similar.compose ~eval1:eval ~eval2:eval ~old_model ~model ~inject a b 
    | Compose_disparate (a, b) ->  Disparate.compose ~eval1:eval ~eval2:eval ~old_model ~model ~inject a b 
    | Dict {t; comparator; r_by_k; m_by_k} -> 
        let T = r_by_k in 
        let T = m_by_k in 
        let old_model = Incr.map old_model ~f:(function
          | Some m -> m
          | None -> Map.empty comparator
        ) in
        let model_and_old_model_map = Incr.Map.merge model old_model ~f:(fun ~key:_ -> function 
          | `Left model -> Some (model, None)
          | `Right _ -> None
          | `Both (model, old_model) -> Some(model, Some old_model))
        in
        let snapshot_map = Incr.Map.mapi' model_and_old_model_map ~f:(fun ~key ~data:model_and_old_model -> 
          let model = Incr.map model_and_old_model ~f:Tuple2.get1 in 
          let old_model = Incr.map model_and_old_model ~f:Tuple2.get2 in 
          let inject action = inject (key, action) in 
          eval ~old_model ~model ~inject t 
        ) in
        let results_map = Incr.Map.mapi snapshot_map 
          ~f:(fun ~key:_ ~data:snapshot -> Snapshot.result snapshot) in
        let apply_action = 
          let%map action_map = Incr.Map.mapi snapshot_map 
            ~f:(fun ~key:_ ~data -> Snapshot.apply_action data)
          and model = model in 
          fun ~schedule_action action -> 
             let id, action = action in 
             let schedule_action a = schedule_action (id, a) in 
             (match Map.find action_map id with
               | None -> model (* drop it on the floor *)
               | Some apply_action ->
                 let data = apply_action ~schedule_action action in
                 Map.set model ~key:id ~data)
        in
        let%map apply_action = apply_action 
        and result = results_map in 
        Snapshot.create ~result ~apply_action
;;

(* Constructor Functions *)
let return r = Constant r 
let of_constant = return
let of_model_map ~f = Dep (Dep.of_mapped ~f)
let of_incr_model_map ~f = Dep (Dep.of_fun ~f)
let of_full ~f = Full f
let of_leaf m = Leaf m


(* rename this *) 
let build_map t ~comparator = 
  let open Core_kernel.Type_equal in
  Dict {t; r_by_k= T; m_by_k= T; comparator }

let map t ~f = Map (t, f)

module Different_model = struct
    let compose a b = Compose_disparate (a, b)

    module Let_syntax = struct 
      let return = return 
      let both = compose
      let map = map
    end
end

module Same_model = struct
    let compose a b = Compose_similar (a, b)
    module Let_syntax = struct 
      let return = return 
      let both = compose
      let map = map
    end
end
