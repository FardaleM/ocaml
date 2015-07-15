(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2015 Institut National de Recherche en Informatique et     *)
(*   en Automatique.  All rights reserved.  This file is distributed      *)
(*   under the terms of the Q Public License version 1.0.                 *)
(*                                                                        *)
(**************************************************************************)

(** Examine a full application of a known closure to determine whether to
    inline.  Then, if inlining is desired, perform inlining using the
    supplied helper functions [inline_by_copying_function_body] and
    [inline_by_copying_function_declaration]. *)
val for_call_site
   : env:Inline_and_simplify_aux.Env.t
  -> r:Inline_and_simplify_aux.Result.t
  -> function_decls:Flambda.function_declarations
  -> lhs_of_application:Variable.t
  -> closure_id_being_applied:Closure_id.t
  -> function_decl:Flambda.function_declaration
  -> value_set_of_closures:Simple_value_approx.value_set_of_closures
  -> args:Variable.t list
  -> args_approxs:Simple_value_approx.t list
  -> dbg:Debuginfo.t
  -> simplify:Inlining_decision_intf.simplify
  -> Flambda.t * Inline_and_simplify_aux.Result.t

(** When a function declaration is encountered in [Flambdainline], the body
    may be subject to inlining immediately, thus changing the declaration.
    This function must return [true] for that to be able to happen. *)
val should_inline_inside_declaration : Flambda.function_declaration -> bool
