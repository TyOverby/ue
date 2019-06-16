open! Ue
open! Ue_web

type tree = Nil | Branch of string * tree * tree

module Tree_renderer = struct
  type model = {opened: bool}

  type action = Toggle_opened

  type result = Vdom.Node.t

  let apply_action ~schedule_action:_ model Toggle_opened =
    {opened= not model.opened}

  let view tree ~inject:_ model =
    let string = match tree with Nil -> "nil" | Branch (s, _, _) -> s in
    let string = string ^ " " ^ string_of_bool model.opened in
    Vdom.Node.text string
end

let tree_leaf_component tree =
  Component.of_functions ~apply_action:Tree_renderer.apply_action
    ~view:(Tree_renderer.view tree)

let rec tree_component tree =
  let open Component.Same_model.Let_syntax in
  let%map a = tree_leaf_component tree and c = tree_component tree in
  Vdom.Node.div [|a; c|]

let _basic_tree = Branch ("hi", Nil, Nil)

let basic_tree =
  Branch ("hi", Branch ("there", Nil, Branch ("world", Nil, Nil)), Nil)

let initial_model = {Tree_renderer.opened= true}

;;
Start.start initial_model (tree_component basic_tree)
