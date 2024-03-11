open Core
open Type
open Level

type unlabeled_tyv =
  | Utyv_prim of prim_ty
  | Utyv_var of string
  | Utyv_sum of labeled_tyv * labeled_tyv
  | Utyv_prod of labeled_tyv * labeled_tyv
  | Utyv_arrow of labeled_tyv * labeled_tyv
  | Utyv_dist of labeled_tyv

and labeled_tyv = Lty of unlabeled_tyv * LevelExpr.t
[@@deriving show, equal, compare, hash]

type context = labeled_tyv String.Map.t

type proc_sigv =
  { psigv_arg_tys : (string * labeled_tyv) list
  ; psigv_ret_ty : labeled_tyv
  }
[@@deriving show]

type global_context =
  { top_external_pure_sigvs : labeled_tyv String.Map.t
  ; top_pure_sigvs : labeled_tyv String.Map.t
  ; top_proc_sigvs : proc_sigv String.Map.t
  }
