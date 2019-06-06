open! Import
open! Core_kernel

type t 

type node_creator 
  =  ?key:string
  -> ?attrs: Attr.t Array.t
  -> t Array.t
  -> t

val div: node_creator
val span: node_creator
val button: node_creator
val span_text: string -> t
val text: string -> t
