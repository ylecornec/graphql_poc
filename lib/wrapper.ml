(**
   This file provides a wrapper around an ocaml-graphql-server [Schema] module
   order to additionaly build a [to_string] function for query [fields].
   These will be used to serialize queries.
   Instead of a string, this could be extented to build a [variables] json objects.
 *)

module Make(Schema: Graphql_intf.Schema) = struct

  module Arg = struct

    type ('obj_arg, 'a ) arg_typ =
      {
        typ: 'obj_arg Schema.Arg.arg_typ;
        to_string: 'a -> string;
      }

    type ('obj_arg, 'a) arg = {
        name: string; (** the name of the argument*)
        doc: string option;
        typ: ('obj_arg, 'a) arg_typ;
      }
    type (_, _, _, _) args =
      | [] : ('ctx, 'out, 'out, string) args
      | ( :: ) : (('a,'to_string_input) arg) * ('ctx, 'out, 'args, 'args_string) args -> ('ctx, 'out, ('a -> 'args), ('to_string_input -> 'args_string)) args


    let rec to_string :
              type ctx out arg args_string. string -> (ctx, out, arg, args_string) args -> string list -> args_string =
      fun name l acc ->
      match l with
      | [] ->
         begin
           match acc with
           | [] -> name
           | _ -> Format.sprintf "%s(%s)" name (String.concat ", " acc)
         end
      | h::t -> fun x -> (to_string name t (Format.asprintf "%s: %s" h.name (h.typ.to_string x)::acc))

    let rec to_string_arg_obj :
              type ctx out arg args_string. string -> (ctx, out, arg, args_string) args -> string list -> args_string =
      fun name l acc ->
      match l with
      | [] ->
         begin
           match acc with
           | [] -> ""
           | _ -> Format.sprintf "{%s}" (String.concat ", " acc)
         end
      | h::t -> fun x -> (to_string_arg_obj name t (Format.asprintf "%s: %s" h.name (h.typ.to_string x)::acc))

    (** build the ocaml-graphql-server [Arg.arg_list]*)
    let rec args_of_myargs :
              type ctx out args_server args_string. (ctx, out, args_server, args_string) args
                   -> (out, args_server) Schema.Arg.arg_list
      = function
      | [] -> Schema.Arg.[]
      | h::t ->
         let graphql_arg = (Schema.Arg.arg ?doc:h.doc h.name ~typ:h.typ.typ) in
         Schema.Arg.(graphql_arg::(args_of_myargs t))

    let int =
      {
        typ = Schema.Arg.int;
        to_string = string_of_int;
      }

    let string =
      {
        typ = Schema.Arg.string;
        to_string = function s -> String.escaped ({|"|}^s^{|"|});
      }

    let float = {
        typ = Schema.Arg.float;
        to_string = string_of_float;
      }

    let bool = {
        typ = Schema.Arg.bool;
        to_string = string_of_bool;
      }

    let guid = {
        typ = Schema.Arg.guid;
        to_string = function s -> String.escaped ({|"|}^s^{|"|});
        (* TODO: Maybe take an int as input of to_sting or (int, string) either *)
      }

    let obj ?doc name ~fields ~coerce ~to_string =
      let build_obj_string = to_string_arg_obj name fields [] in
      let gql_server_fields = args_of_myargs fields in
      let typ = Schema.Arg.obj name ?doc ~fields:gql_server_fields ~coerce in
      {typ; to_string = to_string build_obj_string}

    let non_null (typ: _ arg_typ) =
      {
        typ = Schema.Arg.non_null (typ.typ);
        to_string = typ.to_string;
      }

    let list (typ: _ arg_typ) =
      {
        typ = Schema.Arg.list (typ.typ);
        to_string = Format.asprintf "%a" (Format.pp_print_list (fun fmt s -> Format.fprintf fmt "%s" (typ.to_string s)));
      }

    let enum ?doc name ~values ~to_string =
      {
        typ = Schema.Arg.enum ?doc name ~values;
        to_string;
      }

    let arg ?doc name ~typ = {name; typ; doc;}
  end

  (** a record contraining the ocaml-graphql-server [field], its [name]
      and a [to_string] function to be used when serializing a query *)
  type ('ctx, 'src, 'args_to_string, 'out, 'subquery) field =
    {
      field: ('ctx, 'src) Schema.field;
      to_string: 'args_to_string;
      name: string;
    }

  let field ?doc ?deprecated name ~typ ~(args:(_,'out,_,_) Arg.args) ~resolve : (_ ,_, _,'out,_) field =
    let to_string = Arg.to_string name args [] in
    let args = Arg.args_of_myargs args in
    let field = Schema.field ?doc ?deprecated name ~args:args ~typ ~resolve in
    {name; field; to_string}

  let io_field ?doc ?deprecated name ~typ ~(args:(_,'out,_,_) Arg.args) ~resolve : (_,_, _,'out,_) field =
    let to_string = Arg.to_string name args [] in
    let args = Arg.args_of_myargs args in
    let field = Schema.io_field ?doc ?deprecated name ~args:args ~typ ~resolve in
    {name; field; to_string}


  module Fields = struct
    type ('ctx, 'src, 'acc) fields =
      | [] : ('ctx, 'src, unit) fields
      | ( :: ) : ('ctx, 'src, 'args_to_string, 'out, 'subquery) field * ('ctx, 'src, 'acc) fields -> ('ctx,'src, unit -> 'acc) fields

    let rec to_ocaml_grapql_server_fields :
              type acc. (('ctx, 'src, acc) fields) -> ('ctx, 'src) Schema.field list
      = function
      | [] -> Stdlib.List.[]
      | h::t ->
         (h.field)::(to_ocaml_grapql_server_fields t)
  end

  let obj ?doc name ~(fields:unit -> _ Fields.fields) =
    let fields = lazy (Fields.to_ocaml_grapql_server_fields (fields ())) in
    Schema.obj ?doc name ~fields:(fun _ -> Lazy.force fields)

  let non_null = Schema.non_null
  let string = Schema.string
  let enum = Schema.enum
  let int = Schema.int
  let enum_value = Schema.enum_value

  type ('a, 'b) typ = ('a, 'b) Schema.typ

end
