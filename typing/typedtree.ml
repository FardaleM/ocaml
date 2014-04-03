(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(** Abstract syntax tree after typing *)

(** By comparison with {!Parsetree}:
    - Every {!Longindent.t} is accompanied by a resolved {!Path.t}.

*)

open Misc
open Asttypes
open Types

(* Value expressions for the core language *)

type partial = Partial | Total

(** [optional] is used to pass knowledge of optional arguments up to
    [Translcore.transl_apply].
    When introduced in 2000, this enabled a more efficient code generation for
    optional arguments. However, today the information is redundant as labels
    are passed to [transl_apply] too. Could be cleaned up. *)
type optional = Required | Optional

(** {2 Extension points} *)

type attribute = Parsetree.attribute
       (* [@id ARG]
          [@@id ARG]

          Metadata containers passed around within the AST.
          The compiler ignores unknown attributes.

          See {!Parsetree} for more details.
       *)

type attributes = attribute list

type pattern =
  { pat_desc: pattern_desc;
    pat_loc: Location.t;
    pat_extra : (pat_extra * Location.t * attribute list) list;
    pat_type: type_expr;
    mutable pat_env: Env.t;
    pat_attributes: attribute list;
   }

and pat_extra =
  | Tpat_constraint of core_type
  | Tpat_type of Path.t * Longident.t loc
  | Tpat_unpack

and pattern_desc =
    Tpat_any
        (* _ *)
  | Tpat_var of Ident.t * string loc
        (* x *)
  | Tpat_alias of pattern * Ident.t * string loc
        (* P as 'a *)
  | Tpat_constant of constant
        (* 1, 'a', "true", 1.0, 1l, 1L, 1n *)
  | Tpat_tuple of pattern list
        (* (P1, ..., Pn)

           Invariant: n >= 2
        *)
  | Tpat_construct of
      Longident.t loc * constructor_description * pattern list
        (* C                None
           C P              Some P
           C (P1, ..., Pn)  Some (Ppat_tuple [P1; ...; Pn])
         *)
  | Tpat_variant of label * pattern option * row_desc ref
        (* `A             (None)
           `A P           (Some P)
         *)
  | Tpat_record of
      (Longident.t loc * label_description * pattern) list *
        closed_flag
        (* { l1=P1; ...; ln=Pn }     (flag = Closed)
           { l1=P1; ...; ln=Pn; _}   (flag = Open)

           Invariant: n > 0
         *)
  | Tpat_array of pattern list
        (* [| P1; ...; Pn |] *)
  | Tpat_or of pattern * pattern * row_desc option
        (* P1 | P2 *)
  | Tpat_lazy of pattern
        (* lazy P *)

and expression =
  { exp_desc: expression_desc;
    exp_loc: Location.t;
    exp_extra: (exp_extra * Location.t * attribute list) list;
    exp_type: type_expr;
    exp_env: Env.t;
    exp_attributes: attribute list;
   }

and exp_extra =
  | Texp_constraint of core_type
  | Texp_coerce of core_type option * core_type
  | Texp_open of override_flag * Path.t * Longident.t loc * Env.t
  | Texp_poly of core_type option
  | Texp_newtype of string

and expression_desc =
    Texp_ident of Path.t * Longident.t loc * Types.value_description
        (* x
           M.x
         *)
  | Texp_constant of constant
        (* 1, 'a', "true", 1.0, 1l, 1L, 1n *)
  | Texp_let of rec_flag * value_binding list * expression
        (* let P1 = E1 and ... and Pn = EN in E       (flag = Nonrecursive)
           let rec P1 = E1 and ... and Pn = EN in E   (flag = Recursive)
         *)
  | Texp_function of arg_label * case list * partial
        (* function P1 -> E1 | ... | Pn -> En
           partial =
             Partial if the pattern match is partial
             Total otherwise.
        *)
  | Texp_apply of expression * (arg_label * expression option * optional) list
        (* E0 ~l1:E1 ... ~ln:En
           li can be empty (non labeled argument) or start with '?'
           (optional argument).

           if optional = Optional, the label must start with "?".

           The expression can be None if the expression is abstracted over
           this argument. It currently appears when a label is applied.

           For example:
           let f x ~y = x + y in
           f ~y:3

           The resulting typedtree for the application is:
           Texp_apply (Texp_ident "f/1037",
                       [("", None); ("bar", Texp_constant Const_int 3)])

           Note: As opposed to the parsetree, if an optional label is used in
           the non optional form, the label argument still starts with "?".
         *)
  | Texp_match of expression * case list * case list * partial
  | Texp_try of expression * case list
  | Texp_tuple of expression list
  | Texp_construct of
      Longident.t loc * constructor_description * expression list
        (* C                []
           C E              [E]
           C (E1, ..., En)  [E1;...;En]
        *)
  | Texp_variant of label * expression option
  | Texp_record of
      (Longident.t loc * label_description * expression) list *
        expression option
  | Texp_field of expression * Longident.t loc * label_description
  | Texp_setfield of
      expression * Longident.t loc * label_description * expression
  | Texp_array of expression list
  | Texp_ifthenelse of expression * expression * expression option
  | Texp_sequence of expression * expression
  | Texp_while of expression * expression
  | Texp_for of
      Ident.t * Parsetree.pattern * expression * expression * direction_flag *
        expression
  | Texp_send of expression * meth * expression option
  | Texp_new of Path.t * Longident.t loc * Types.class_declaration
  | Texp_instvar of Path.t * Path.t * string loc
  | Texp_setinstvar of Path.t * Path.t * string loc * expression
  | Texp_override of Path.t * (Path.t * string loc * expression) list
  | Texp_letmodule of Ident.t * string loc * module_expr * expression
  | Texp_assert of expression
  | Texp_lazy of expression
  | Texp_object of class_structure * string list
  | Texp_pack of module_expr
  | Texp_unreachable

and meth =
    Tmeth_name of string
  | Tmeth_val of Ident.t

and case =
    {
     c_lhs: pattern;
     c_guard: expression option;
     c_rhs: expression;
    }

(* Value expressions for the class language *)

and class_expr =
    {
     cl_desc: class_expr_desc;
     cl_loc: Location.t;
     cl_type: Types.class_type;
     cl_env: Env.t;
     cl_attributes: attribute list;
    }

and class_expr_desc =
    Tcl_ident of Path.t * Longident.t loc * core_type list
  | Tcl_structure of class_structure
  | Tcl_fun of
      arg_label * pattern * (Ident.t * string loc * expression) list
      * class_expr * partial
  | Tcl_apply of class_expr * (arg_label * expression option * optional) list
  | Tcl_let of rec_flag * value_binding list *
                  (Ident.t * string loc * expression) list * class_expr
  | Tcl_constraint of
      class_expr * class_type option * string list * string list * Concr.t
    (* Visible instance variables, methods and concretes methods *)

and class_structure =
  {
   cstr_self: pattern;
   cstr_fields: class_field list;
   cstr_type: Types.class_signature;
   cstr_meths: Ident.t Meths.t;
  }

and class_field =
   {
    cf_desc: class_field_desc;
    cf_loc: Location.t;
    cf_attributes: attribute list;
  }

and class_field_kind =
  | Tcfk_virtual of core_type
  | Tcfk_concrete of override_flag * expression

and class_field_desc =
    Tcf_inherit of
      override_flag * class_expr * string option * (string * Ident.t) list *
        (string * Ident.t) list
    (* Inherited instance variables and concrete methods *)
  | Tcf_val of string loc * mutable_flag * Ident.t * class_field_kind * bool
  | Tcf_method of string loc * private_flag * class_field_kind
  | Tcf_constraint of core_type * core_type
  | Tcf_initializer of expression
  | Tcf_attribute of attribute

(* Value expressions for the module language *)

and module_expr =
  { mod_desc: module_expr_desc;
    mod_loc: Location.t;
    mod_type: Types.module_type;
    mod_env: Env.t;
    mod_attributes: attribute list;
   }

(** Annotations for [Tmod_constraint]. *)
and module_type_constraint =
  | Tmodtype_implicit
  (* The module type constraint has been synthesized during typecheking. *)
  | Tmodtype_explicit of module_type
  (* The module type was in the source file. *)

and module_expr_desc =
    Tmod_ident of Path.t * Longident.t loc
  | Tmod_structure of structure
  | Tmod_functor of Ident.t * string loc * module_type option * module_expr
  | Tmod_apply of module_expr * module_expr * module_coercion
  | Tmod_constraint of
      module_expr * Types.module_type * module_type_constraint * module_coercion
    (* ME          (constraint = Tmodtype_implicit)
       (ME : MT)   (constraint = Tmodtype_explicit MT)
    *)
  | Tmod_unpack of expression * Types.module_type

and structure = {
  str_items : structure_item list;
  str_type : Types.signature;
  str_final_env : Env.t;
}

and structure_item =
  { str_desc : structure_item_desc;
    str_loc : Location.t;
    str_env : Env.t
  }

and structure_item_desc =
    Tstr_eval of expression * attributes
  | Tstr_value of rec_flag * value_binding list
  | Tstr_primitive of value_description
  | Tstr_type of rec_flag * type_declaration list
  | Tstr_typext of type_extension
  | Tstr_exception of extension_constructor
  | Tstr_module of module_binding
  | Tstr_recmodule of module_binding list
  | Tstr_modtype of module_type_declaration
  | Tstr_open of open_description
  | Tstr_class of (class_declaration * string list) list
  | Tstr_class_type of (Ident.t * string loc * class_type_declaration) list
  | Tstr_include of include_declaration
  | Tstr_attribute of attribute

and module_binding =
    {
     mb_id: Ident.t;
     mb_name: string loc;
     mb_expr: module_expr;
     mb_attributes: attribute list;
     mb_loc: Location.t;
    }

and value_binding =
  {
    vb_pat: pattern;
    vb_expr: expression;
    vb_attributes: attributes;
    vb_loc: Location.t;
  }

and module_coercion =
    Tcoerce_none
  | Tcoerce_structure of (int * module_coercion) list *
                         (Ident.t * int * module_coercion) list
  | Tcoerce_functor of module_coercion * module_coercion
  | Tcoerce_primitive of primitive_coercion
  | Tcoerce_alias of Path.t * module_coercion

and module_type =
  { mty_desc: module_type_desc;
    mty_type : Types.module_type;
    mty_env : Env.t;
    mty_loc: Location.t;
    mty_attributes: attribute list;
   }

and module_type_desc =
    Tmty_ident of Path.t * Longident.t loc
  | Tmty_signature of signature
  | Tmty_functor of Ident.t * string loc * module_type option * module_type
  | Tmty_with of module_type * (Path.t * Longident.t loc * with_constraint) list
  | Tmty_typeof of module_expr
  | Tmty_alias of Path.t * Longident.t loc

(* Keep primitive type information for type-based lambda-code specialization *)
and primitive_coercion =
  {
    pc_desc: Primitive.description;
    pc_type: type_expr;
    pc_env: Env.t;
    pc_loc : Location.t;
  }

and signature = {
  sig_items : signature_item list;
  sig_type : Types.signature;
  sig_final_env : Env.t;
}

and signature_item =
  { sig_desc: signature_item_desc;
    sig_env : Env.t; (* BINANNOT ADDED *)
    sig_loc: Location.t }

and signature_item_desc =
    Tsig_value of value_description
  | Tsig_type of rec_flag * type_declaration list
  | Tsig_typext of type_extension
  | Tsig_exception of extension_constructor
  | Tsig_module of module_declaration
  | Tsig_recmodule of module_declaration list
  | Tsig_modtype of module_type_declaration
  | Tsig_open of open_description
  | Tsig_include of include_description
  | Tsig_class of class_description list
  | Tsig_class_type of class_type_declaration list
  | Tsig_attribute of attribute

and module_declaration =
    {
     md_id: Ident.t;
     md_name: string loc;
     md_type: module_type;
     md_attributes: attribute list;
     md_loc: Location.t;
    }

and module_type_declaration =
    {
     mtd_id: Ident.t;
     mtd_name: string loc;
     mtd_type: module_type option;
     mtd_attributes: attribute list;
     mtd_loc: Location.t;
    }

and open_description =
    {
     open_path: Path.t;
     open_txt: Longident.t loc;
     open_override: override_flag;
     open_loc: Location.t;
     open_attributes: attribute list;
    }

and 'a include_infos =
    {
     incl_mod: 'a;
     incl_type: Types.signature;
     incl_loc: Location.t;
     incl_attributes: attribute list;
    }

and include_description = module_type include_infos

and include_declaration = module_expr include_infos

and with_constraint =
    Twith_type of type_declaration
  | Twith_module of Path.t * Longident.t loc
  | Twith_typesubst of type_declaration
  | Twith_modsubst of Path.t * Longident.t loc

and core_type =
(* mutable because of [Typeclass.declare_method] *)
  { mutable ctyp_desc : core_type_desc;
    mutable ctyp_type : type_expr;
    ctyp_env : Env.t; (* BINANNOT ADDED *)
    ctyp_loc : Location.t;
    ctyp_attributes: attribute list;
   }

and core_type_desc =
    Ttyp_any
  | Ttyp_var of string
  | Ttyp_arrow of arg_label * core_type * core_type
  | Ttyp_tuple of core_type list
  | Ttyp_constr of Path.t * Longident.t loc * core_type list
  | Ttyp_object of (string * attributes * core_type) list * closed_flag
  | Ttyp_class of Path.t * Longident.t loc * core_type list
  | Ttyp_alias of core_type * string
  | Ttyp_variant of row_field list * closed_flag * label list option
  | Ttyp_poly of string list * core_type
  | Ttyp_package of package_type

and package_type = {
  pack_path : Path.t;
  pack_fields : (Longident.t loc * core_type) list;
  pack_type : Types.module_type;
  pack_txt : Longident.t loc;
}

and row_field =
    Ttag of label * attributes * bool * core_type list
  | Tinherit of core_type

and value_description =
  { val_id: Ident.t;
    val_name: string loc;
    val_desc: core_type;
    val_val: Types.value_description;
    val_prim: string list;
    val_loc: Location.t;
    val_attributes: attribute list;
    }

and type_declaration =
  { typ_id: Ident.t;
    typ_name: string loc;
    typ_params: (core_type * variance) list;
    typ_type: Types.type_declaration;
    typ_cstrs: (core_type * core_type * Location.t) list;
    typ_kind: type_kind;
    typ_private: private_flag;
    typ_manifest: core_type option;
    typ_loc: Location.t;
    typ_attributes: attribute list;
   }

and type_kind =
    Ttype_abstract
  | Ttype_variant of constructor_declaration list
  | Ttype_record of label_declaration list
  | Ttype_open

and label_declaration =
    {
     ld_id: Ident.t;
     ld_name: string loc;
     ld_mutable: mutable_flag;
     ld_type: core_type;
     ld_loc: Location.t;
     ld_attributes: attribute list;
    }

and constructor_declaration =
    {
     cd_id: Ident.t;
     cd_name: string loc;
     cd_args: constructor_arguments;
     cd_res: core_type option;
     cd_loc: Location.t;
     cd_attributes: attribute list;
    }

and constructor_arguments =
  | Cstr_tuple of core_type list
  | Cstr_record of label_declaration list

and type_extension =
  {
    tyext_path: Path.t;
    tyext_txt: Longident.t loc;
    tyext_params: (core_type * variance) list;
    tyext_constructors: extension_constructor list;
    tyext_private: private_flag;
    tyext_attributes: attribute list;
  }

and extension_constructor =
  {
    ext_id: Ident.t;
    ext_name: string loc;
    ext_type: Types.extension_constructor;
    ext_kind: extension_constructor_kind;
    ext_loc: Location.t;
    ext_attributes: attribute list;
  }

and extension_constructor_kind =
    Text_decl of constructor_arguments * core_type option
  | Text_rebind of Path.t * Longident.t loc

and class_type =
    {
     cltyp_desc: class_type_desc;
     cltyp_type: Types.class_type;
     cltyp_env: Env.t;
     cltyp_loc: Location.t;
     cltyp_attributes: attribute list;
    }

and class_type_desc =
    Tcty_constr of Path.t * Longident.t loc * core_type list
  | Tcty_signature of class_signature
  | Tcty_arrow of arg_label * core_type * class_type

and class_signature = {
    csig_self: core_type;
    csig_fields: class_type_field list;
    csig_type: Types.class_signature;
  }

and class_type_field = {
    ctf_desc: class_type_field_desc;
    ctf_loc: Location.t;
    ctf_attributes: attribute list;
  }

and class_type_field_desc =
  | Tctf_inherit of class_type
  | Tctf_val of (string * mutable_flag * virtual_flag * core_type)
  | Tctf_method of (string * private_flag * virtual_flag * core_type)
  | Tctf_constraint of (core_type * core_type)
  | Tctf_attribute of attribute

and class_declaration =
  class_expr class_infos

and class_description =
  class_type class_infos

and class_type_declaration =
  class_type class_infos

and 'a class_infos =
  { ci_virt: virtual_flag;
    ci_params: (core_type * variance) list;
    ci_id_name: string loc;
    ci_id_class: Ident.t;
    ci_id_class_type: Ident.t;
    ci_id_object: Ident.t;
    ci_id_typesharp: Ident.t;
    ci_expr: 'a;
    ci_decl: Types.class_declaration;
    ci_type_decl: Types.class_type_declaration;
    ci_loc: Location.t;
    ci_attributes: attribute list;
   }

(* Auxiliary functions over the a.s.t. *)

let iter_pattern_desc f = function
  | Tpat_alias(p, _, _) -> f p
  | Tpat_tuple patl -> List.iter f patl
  | Tpat_construct(_, cstr, patl) -> List.iter f patl
  | Tpat_variant(_, pat, _) -> may f pat
  | Tpat_record (lbl_pat_list, _) ->
      List.iter (fun (_, lbl, pat) -> f pat) lbl_pat_list
  | Tpat_array patl -> List.iter f patl
  | Tpat_or(p1, p2, _) -> f p1; f p2
  | Tpat_lazy p -> f p
  | Tpat_any
  | Tpat_var _
  | Tpat_constant _ -> ()

let map_pattern_desc f d =
  match d with
  | Tpat_alias (p1, id, s) ->
      Tpat_alias (f p1, id, s)
  | Tpat_tuple pats ->
      Tpat_tuple (List.map f pats)
  | Tpat_record (lpats, closed) ->
      Tpat_record (List.map (fun (lid, l,p) -> lid, l, f p) lpats, closed)
  | Tpat_construct (lid, c,pats) ->
      Tpat_construct (lid, c, List.map f pats)
  | Tpat_array pats ->
      Tpat_array (List.map f pats)
  | Tpat_lazy p1 -> Tpat_lazy (f p1)
  | Tpat_variant (x1, Some p1, x2) ->
      Tpat_variant (x1, Some (f p1), x2)
  | Tpat_or (p1,p2,path) ->
      Tpat_or (f p1, f p2, path)
  | Tpat_var _
  | Tpat_constant _
  | Tpat_any
  | Tpat_variant (_,None,_) -> d

(* List the identifiers bound by a pattern or a let *)

let idents = ref([]: (Ident.t * string loc) list)

let rec bound_idents pat =
  match pat.pat_desc with
  | Tpat_var (id,s) -> idents := (id,s) :: !idents
  | Tpat_alias(p, id, s ) ->
      bound_idents p; idents := (id,s) :: !idents
  | Tpat_or(p1, _, _) ->
      (* Invariant : both arguments binds the same variables *)
      bound_idents p1
  | d -> iter_pattern_desc bound_idents d

let pat_bound_idents pat =
  idents := []; bound_idents pat; let res = !idents in idents := []; res

let rev_let_bound_idents_with_loc bindings =
  idents := [];
  List.iter (fun vb -> bound_idents vb.vb_pat) bindings;
  let res = !idents in idents := []; res

let let_bound_idents_with_loc pat_expr_list =
  List.rev(rev_let_bound_idents_with_loc pat_expr_list)

let rev_let_bound_idents pat = List.map fst (rev_let_bound_idents_with_loc pat)
let let_bound_idents pat = List.map  fst (let_bound_idents_with_loc pat)

let alpha_var env id = List.assoc id env

let rec alpha_pat env p = match p.pat_desc with
| Tpat_var (id, s) -> (* note the ``Not_found'' case *)
    {p with pat_desc =
     try Tpat_var (alpha_var env id, s) with
     | Not_found -> Tpat_any}
| Tpat_alias (p1, id, s) ->
    let new_p =  alpha_pat env p1 in
    begin try
      {p with pat_desc = Tpat_alias (new_p, alpha_var env id, s)}
    with
    | Not_found -> new_p
    end
| d ->
    {p with pat_desc = map_pattern_desc (alpha_pat env) d}

let mkloc = Location.mkloc
let mknoloc = Location.mknoloc
