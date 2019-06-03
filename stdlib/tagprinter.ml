module Index = struct
  type descriptor = Obj.Tag_descriptor.t

  let profinfo_mask = (1 lsl Obj.profinfo_bits ()) - 1
  let descriptor_index x = profinfo_mask land (Obj.Tag_descriptor.hash x)

  type t = {
    descriptors : (int, descriptor list) Hashtbl.t;
    variants : (int, string list) Hashtbl.t;
  }

  let make () : t = {
    descriptors = Hashtbl.create 17;
    variants = Hashtbl.create 17;
  }

  let register (t : t) = function
    | Obj.Tag_descriptor.Polymorphic_variant_constant name ->
        let i = Obj.Tag_descriptor.hash_variant name in
        begin match Hashtbl.find t.variants i with
        | exception Not_found -> Hashtbl.add t.variants i [name]
        | names -> Hashtbl.replace t.variants i (name :: names)
        end
    | tag ->
        let i = descriptor_index tag in
        begin match Hashtbl.find t.descriptors i with
        | exception Not_found -> Hashtbl.add t.descriptors i [tag]
        | tags -> Hashtbl.replace t.descriptors i (tag :: tags)
        end

  let register_list t tags =
    List.iter (register t) tags

  let lookup (t : t) i =
    try Hashtbl.find t.descriptors i
    with Not_found -> []

  let lookup_by_profinfo t o =
    lookup t (Obj.get_profinfo o)

  let lookup_variant (t : t) i =
    try Hashtbl.find t.variants i
    with Not_found -> []

  let self_index = lazy (make ())
  let self_descriptors = ref []

  let self_index () =
    let lazy index = self_index in
    let new_descriptors = Obj.Tag_descriptor.read_self_descriptors () in
    let last_descriptors = !self_descriptors in
    if new_descriptors <> last_descriptors then (
      self_descriptors := new_descriptors;
      let rec update_descriptors = function
        | descriptors when descriptors == last_descriptors -> ()
        | [] -> ()
        | x :: xs ->
          register index x;
          update_descriptors xs
      in
      update_descriptors new_descriptors
    );
    index
end

module Introspect = struct
  type 'a fields = int * (int -> 'a)
  let field_count (count, _ : _ fields) = count
  let field_get (_, getter : _ fields) i = getter i

  type approx = Obj.Tag_descriptor.approx =
    | Any
    | Char
    | Int
    | Constants of string array
    | Polymorphic_variants

  type dynobj = approx * Obj.t
  let get_approx (approx, _ : dynobj) = approx
  let get_obj (_, obj : dynobj) = obj

  let no_approx (obj : Obj.t) : dynobj = (Obj.Tag_descriptor.Any, obj)
  let lift ?(approx=Obj.Tag_descriptor.Any) (obj : Obj.t) : dynobj =
    (approx, obj)

  type dynval =
    | String of string
    (* [String "foo"] = "foo" *)
    | Float of float
    (* [Float 12.12] = 12.12 *)
    | Char of char
    (* [Char 'c'] = 'x' *)
    | Int_or_constant of int * string list
    (* [Int_or_constant (1, ["`Bla"])] = 1 or `Bla *)
    | Constant of string list
    (* [Constant ["`Bla"]] = `Bla *)
    | Array of dynobj fields
    (* [Array f] = [|f0, f1, f2, ...|] *)
    | Tuple of { name: string; fields: dynobj fields }
    (* [Tuple f] = (f0, f1, f2, ...) *)
    | Record of { name: string; fields: (string * dynobj) fields }
    (* [Record f] = { fst f0 : snd f0; fst f1 : snd f1; ... } *)
    | Polymorphic_variant of string * dynobj
    | Closure | Lazy | Abstract | Custom | Unknown

  let double_to_wo_shift = match Sys.word_size with
    | 64 -> 0
    | _  -> 1

  let fields_of_block f obj =
    if Obj.tag obj = Obj.double_array_tag then
      (Obj.size obj lsr double_to_wo_shift,
       fun i -> f i (Obj.repr (Obj.double_field obj i)))
    else
      (Obj.size obj, fun i -> f i (Obj.field obj i))

  let find_tag t obj =
    let otag = Obj.tag obj in
    if otag = Obj.int_tag then None
    else
      let osize = Obj.size obj in
      let select = function
        | Obj.Tag_descriptor.Array _ -> true
        | Obj.Tag_descriptor.Polymorphic_variant -> osize = 2
        | Obj.Tag_descriptor.Tuple t ->
            otag = t.tag && osize = Array.length t.fields
        | Obj.Tag_descriptor.Record t ->
            otag = t.tag &&
            let len = Array.length t.fields in
            let len =
              if otag = Obj.double_array_tag
              then len lsl double_to_wo_shift else len
            in
            osize = len
        | Obj.Tag_descriptor.Polymorphic_variant_constant _ -> false
        | Obj.Tag_descriptor.Unknown -> false
      in
      List.find_opt select (Index.lookup_by_profinfo t obj)

  let no_approx' (_ : int) (obj : Obj.t) = (Obj.Tag_descriptor.Any, obj)

  let raw_dynval (obj : Obj.t) =
    if Obj.is_int obj then
      Int_or_constant (Obj.obj obj, [])
    else
      let tag = Obj.tag obj in
      if tag <= Obj.last_non_constant_constructor_tag then (
        if tag = 0
        then Tuple { name = ""; fields = fields_of_block no_approx' obj }
        else Tuple { name = "Tag#" ^ string_of_int tag;
                     fields = fields_of_block no_approx' obj }
      ) else if tag = Obj.string_tag then
        String (Obj.obj obj)
      else if tag = Obj.double_tag then
        Float (Obj.obj obj)
      else if tag = Obj.double_array_tag then
        Array (fields_of_block no_approx' obj)
      else if tag = Obj.closure_tag then
        Closure
      else if tag = Obj.lazy_tag then
        Lazy
      else if tag = Obj.abstract_tag then
        Abstract
      else if tag = Obj.custom_tag then
        Custom
      else
        Unknown

  let dynval t (approx, obj) =
    if Obj.is_int obj then
      let i = (Obj.obj obj : int) in
      match approx with
      | Obj.Tag_descriptor.Any ->
          Int_or_constant (i, Index.lookup_variant t i)
      | Obj.Tag_descriptor.Int ->
          Int_or_constant (i, [])
      | Obj.Tag_descriptor.Char ->
          (try Char (Char.chr i) with _ -> Int_or_constant (i, []))
      | Obj.Tag_descriptor.Constants names ->
          if i >= 0 && i < Array.length names
          then Constant [names.(i)]
          else Int_or_constant (i, [])
      | Obj.Tag_descriptor.Polymorphic_variants ->
          begin match Index.lookup_variant t i with
          | [] -> Int_or_constant (i, [])
          | names -> Constant names
          end
    else
      match find_tag t obj with
      | Some (Obj.Tag_descriptor.Array approx ) ->
          Array (fields_of_block (fun _ obj -> approx, obj) obj)
      | Some (Obj.Tag_descriptor.Record {name; fields}) ->
          let get_field i obj =
            let fname, fapprox = fields.(i) in
            (fname, (fapprox, obj))
          in
          Record { name; fields = fields_of_block get_field obj }
      | Some (Obj.Tag_descriptor.Tuple {name; fields}) ->
          let get_field i obj = (fields.(i), obj) in
          Tuple {name; fields = fields_of_block get_field obj}
      | Some (Obj.Tag_descriptor.Polymorphic_variant) ->
          let name = (Obj.obj (Obj.field obj 0) : int) in
          let payload = Obj.field obj 1 in
          begin match Index.lookup_variant t name with
          | [] -> Polymorphic_variant (string_of_int name, no_approx payload)
          | name :: _ -> Polymorphic_variant (name, no_approx payload)
          end
      | Some (Obj.Tag_descriptor.Unknown)
      | Some (Obj.Tag_descriptor.Polymorphic_variant_constant _)
      | None -> raw_dynval obj

  let self_dynval dynobj = dynval (Index.self_index ()) dynobj
end
