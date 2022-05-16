(** This module defines a type for an address and a Gql module for graphql features related to this type *)

type t =
  {
    mutable kind: Kind.t;
    number: int;
    name: string;
  } [@@deriving yojson]

let dummy1 = {
    kind = Kind.Kind1;
    number = 1;
    name = "dummy_addresse_1";
  }

let dummy2 = {
    kind = Kind.Kind2;
    number = 2;
    name = "dummy_addresse_2";
  }

module Gql =
  struct

    (** The Wrapper functor wraps around a Schema module from ocaml-graphpq-server.
        And provides [to_string] functions for arguments that will be used to serialise queries.*)
    open Wrapper.Make(Graphql_lwt.Schema)
    open Gql_types.Make(Graphql_lwt.Schema)

    type 'a final_option_modifier = 'a
    type 'a modifier = 'a
    type out_before_modifiers = t
    type out = out_before_modifiers modifier final_option_modifier

    type ('kind, 'name, 'setkind, 'kinds) r =
      {
       res_kind: 'kind;
       res_name: 'name;
       res_setkind: 'setkind;
       res_kinds: 'kinds;
      }

    type setkind_args =
      {arg_kind: Kind.t;
       arg_ignore: bool;
       arg_obj: Obj_arg.t option;
       arg_with_default: bool option; (** If arg_with_default is None, the default value will be used *)
      }

    (**
       The type used to build queries, with possible arguments in a type safe way.
       `Kind, `Name, and `SetKind correspond to fields from the [typ] object.
     *)

    module Gql_kinds = ListScalar(NullableScalar(ListScalar(Kind.Gql)))
    module Kind_gql = NullableScalar(Kind.Gql)
    type _ query =
      | Empty: (unit, unit, unit, unit) r query
      | Kind: {siblings: (unit, 'name, 'setkind, 'kinds) r query} ->
              (Kind.Gql.out, 'name, 'setkind, 'kinds) r query
      | Kinds: {siblings: ('kind, 'name, 'setkind, unit) r query} ->
              ('kind, 'name, 'setkind, Gql_kinds.out) r query
      | Name: {siblings: ('kind, unit, 'setkind, 'kinds) r query} ->
              ('kind, string, 'setkind, 'kinds) r query
      | Setkind: {
          siblings: ('kind, 'name, unit, 'kinds) r query;
          arguments: setkind_args;
          subquery: 'sub query;
        } -> ('kind, 'name, 'sub modifier final_option_modifier, 'kinds) r query

    let kind : (_,_ , string, Kind_gql.out, _) Fields.field [@gql "Address" Kind_gql]=
      field "kind" ~args:[] ~typ:(Kind_gql.typ()) ~resolve:(fun _ t -> Some t.kind)

    let name : (_,_, string, string, _) Fields.field =

      field "name" ~args:[] ~typ:(non_null string) ~resolve:(fun _ (t:t) -> t.name)


    let kinds = field "kinds" ~args:[] ~typ:(Gql_kinds.typ()) ~resolve:(fun _ t -> [Some ([t.kind; t.kind])])

    (** There can be mutual recursion between the graphql [set_kind] field and [typ] object typ.*)

    let rec set_kind () : (unit, t, Kind.t -> bool -> Obj_arg.t option -> bool option -> string, t , 'a) Fields.field
      =
      let args =
        Arg.[
            arg "kind" ~typ:(non_null @@ Kind.arg_typ ());
            arg "ignore" ~typ:(non_null bool);
            arg "obj_arg" ~typ:(Obj_arg.arg_typ());
            arg' "arg_with_default" ~typ:bool ~doc:"testing default values in wrapper" ~default:true;
       ]
      in
      field "setKind"
        ~typ:(typ (): (_, t) typ)
        ~resolve: (fun _ (t:t) new_kind _ignore obj_arg arg_with_default ->
          let () =
            match obj_arg with
            | Some (Ok obj_arg) -> Format.printf "resolving setking obj_arg = %a\n" Obj_arg.pp obj_arg
            | _ -> Format.printf "obj_arg failure\n"
          in
          let () = Format.printf "resolving setking arg_with_default = %b\n" arg_with_default in
          let () = Format.print_flush () in
          let () = if not _ignore then t.kind <- new_kind in
          t
        )
        ~args

    and typ_nullable () = 
      obj "Address" ~fields: (fun _ -> Fields.(([kind; set_kind (); name; kinds]: _ fields)))
    and typ () = non_null @@ typ_nullable ()

    let rec response_of_json: type a. a query -> Yojson.Basic.t -> a modifier final_option_modifier
      = fun query json ->
      let rec aux: type a.a query -> Yojson.Basic.t -> a =
        fun query json ->
        match query with
        | Kind {siblings} ->
           { (aux siblings json) with res_kind = Kind.Gql.response_of_json (Json.get kind.name json) }
        | Kinds {siblings} ->
           { (aux siblings json) with res_kinds = Gql_kinds.response_of_json (Json.get kinds.name json) }
        | Empty -> {res_kind = (); res_name = (); res_setkind = (); res_kinds = ()}
        | Name {siblings} ->
           { (aux siblings json) with res_name = Gql_string.response_of_json @@ Json.get name.name json }
        | Setkind {siblings; subquery; _} ->
           { (aux siblings json) with res_setkind = response_of_json subquery (Json.get (set_kind()).name json) }
      in
      aux query json

    (** The mk_query function generates the query to send to the server.
        For now it outputs a string but it should likely output json and a [variables] json object.*)

    let rec mk_query : type a. a query -> string =
      function query -> 
        let rec build_fields : type a. a query -> string list = function
          | Empty -> []
          | Kind {siblings; _} -> kind.to_string::(build_fields siblings)
          | Kinds {siblings; _} -> kinds.to_string::(build_fields siblings)
          | Name {siblings; _} -> name.to_string::(build_fields siblings)
          | Setkind {siblings; arguments; subquery;_} ->
             (Format.asprintf "%s {%s}"
                ((set_kind()).to_string arguments.arg_kind arguments.arg_ignore arguments.arg_obj arguments.arg_with_default) (** arguments are in the same order as they are declared *)
                (mk_query subquery))::(build_fields siblings)
        in
        Stdlib.String.concat " " @@ build_fields query
  end

