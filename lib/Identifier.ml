open Core
module Location = Common.Location_ext

type 'a loc = 'a Location.loc =
  { txt : 'a
  ; loc : Location.t
  }
[@@deriving show, equal, compare, hash]

type variable_id = string loc [@@deriving show, equal, compare, hash]
type def_id = string loc [@@deriving show, equal, compare, hash]
type fmtnel_id = string loc [@@deriving show, equal, compare, hash]
type type_id = string loc [@@deriving show, equal, compare, hash]

let print_id fmt id = Format.fprintf fmt "%s" id.txt
let make_id (s : string) : string loc = Location.mknoloc s
