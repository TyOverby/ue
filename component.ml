open! Core_kernel
open! Import

open Incr.Let_syntax

module Dep = struct 
    type ('result, 'model) t = 'model Incr.t -> 'result Incr.t 

    let of_fun ~f = f
    let of_mapped ~f = Incr.map ~f
end

module Full = struct
    type ('result, 'action, 'model) t 
        =  old_model: 'model Incr.t
        -> model: 'model Incr.t
        -> inject: ('action -> Event.t)
        -> ('result, 'action, 'model) Snapshot.t Incr.t
end

module Similar_join = struct 
    type 'a t = 'a * 'a
end

module Disparate_join = struct
   type ('a, 'b, 'c, 'd) s 
   type ('r1, 'r2, 'a1, 'a2, 'm1, 'm2, 's1, 's2) t = ('r1, 'a1, 'm1, 's1)s * ('r2, 'a2, 'm2, 's2)s
end

type ('result, 'action, 'model) t =
    | Constant : 'result -> ('result, Nothing.t, _) t
    | Dep :  ('result, 'model) Dep.t -> ('result, Nothing.t, 'model) t
    | Full : ('result, 'action, 'model) Full.t -> ('result, 'action, 'model) t
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
  =  old_model:'model Incr.t 
  -> model:'model Incr.t 
  -> inject:('action -> Event.t) 
  -> ('result, 'action, 'model) t 
  -> ('result, 'action, 'model) Snapshot.t Incr.t 

module Similar = struct 
    type ('r1, 'r2, 'a1, 'a2, 'm) compose_type 
      =  eval1:('r1, 'a1, 'm) eval_type 
      -> eval2:('r2, 'a2, 'm) eval_type 
      -> old_model:'m Incr.t
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
      let apply_action action ~schedule_action = 
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
      -> old_model:('m1 * 'm2) Incr.t
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
      let old_model_a = Incr.map old_model ~f:(Tuple2.get1) in 
      let old_model_b = Incr.map old_model ~f:(Tuple2.get2) in 

      let%map   a = eval1 ~old_model:old_model_a ~model:model_a ~inject:inject_a a
      and       b = eval2 ~old_model:old_model_b ~model:model_b ~inject:inject_b b 
      and model_a = model_a 
      and model_b = model_b in 
      let apply_action action  ~schedule_action = 
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

(* GADTS ARE TRULY THE BEST OH LAWDY *)
let rec eval: type r a m . (r, a, m) eval_type = 
    fun ~old_model ~model ~inject -> function 
    | Constant result -> Incr.const (Snapshot.create ~result ~apply_action:Nothing.unreachable_code)
    | Dep f -> 
      let%map result = f model in
      Snapshot.create ~result ~apply_action:Nothing.unreachable_code
    | Full f -> f ~old_model ~model ~inject
    | Compose_similar (a, b) -> Similar.compose ~eval1:eval ~eval2:eval ~old_model ~model ~inject a b 
    | Compose_disparate (a, b) -> Disparate.compose ~eval1:eval ~eval2:eval ~old_model ~model ~inject a b 
