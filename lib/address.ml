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
    open Graphql_lwt.Schema

    type 'a res = 'a option
    type out = t res

    (**
       The type used to build queries, with possible arguments in a type safe way.

       `Kind, `Name, and `SetKind correspond to fields from the [typ] object.

       However `GetAll correspond to a more complex query [GetAll.q] that we can parse the result of into a [GetAll.response] structure.
       In practice we could use the [fields_derivers] mechanism to build the [GetAll] module for this. 
     *)


    open Gql_types


    (** these types [r], [setkind_args] and [query] must match with the types of the various fields below.
       In order to generate them, maybe we could require the user to explicitely type these fields and then read this information in a ppx. *)

    type ('kind, 'name, 'setkind) r =
      {
       res_kind: 'kind;
       res_name: 'name;
       res_setkind: 'setkind;
      }

    type setkind_args =
      {arg_kind: Kind.t;
       arg_ignore: bool;}

    type _ query =
      | Empty: (unit, unit, unit) r query
      | Kind: {siblings: (unit, 'name, 'setkind) r query} ->
              (Kind.Gql.out, 'name, 'setkind) r query
      | Name: {siblings: ('kind, unit, 'setkind) r query} ->
              ('kind, string, 'setkind) r query
      | Setkind: {
          siblings: ('kind, 'name, unit) r query;
          arguments: setkind_args;
          subquery: 'sub query;
        } -> ('kind, 'name, 'sub res) r query

    (** These fields would be defined manually, or maybe via field_derivers.
        I wonder if it is possible to use these explicit type annotations to provide information to the ppx.*)

    let kind : (_, string, Kind.Gql.out, no_subquery) Gql_fields.field =
      Gql_fields.field "kind" ~args:[] ~typ:(Kind.Gql.typ()) ~resolve:(fun _ t -> Some t.kind)

    let name : (_, string, string, no_subquery) Gql_fields.field =
      Gql_fields.field "name" ~args:[] ~typ:(non_null string) ~resolve:(fun _ t -> t.name)

    (** There can be mutual recursion between the graphql [set_kind] field and [typ] object typ.*)

    let rec set_kind (): 
              (_, Kind.t -> bool -> string, out, _ query)
                Gql_fields.field =
      (** The type of [args] from [Gql_fields.MyArg] is used as the [arg_list] type from ocaml-graphql-server,
          but we must provide [to_string] functions that will be used to serialize queries as well *)
      let args =
        Gql_fields.MyArg.[
            { name = "kind";
             typ = Arg.non_null @@ Kind.Gql.arg_typ ();
             to_string = Kind.show;};

            { name = "ignore";
             typ = Arg.(non_null bool);
             to_string = string_of_bool;}
       ]
      in
      Gql_fields.field "setKind"
        ~typ:(typ (): (_, t option) typ)
        ~resolve: (fun _ (t:t) new_kind _ignore ->
          let () = if not _ignore then t.kind <- new_kind in
          Some t
        )
        ~args

    and typ () = 
      obj "Address"
        ~fields:(fun _ -> [
                     kind.field ();
                     (set_kind()).field ();
                     name.field ();
        ])


    let rec response_of_json: type a. a query -> Yojson.Basic.t -> a res
      = fun query json ->
      let rec aux: type a.a query -> Yojson.Basic.t -> a =
        fun query json ->
        match query with
        | Kind {siblings} ->
           { (aux siblings json) with res_kind = Kind.Gql.response_of_json (Json.get kind.name json) }
        | Empty -> {res_kind = (); res_name = (); res_setkind = ();}
        | Name {siblings} ->
           { (aux siblings json) with res_name = Json.get_string @@ Json.get name.name json }
        | Setkind {siblings; subquery; _} ->
           { (aux siblings json) with res_setkind = response_of_json subquery (Json.get (set_kind()).name json) }
      in
      match json with
      | `Null -> None
      | _ -> Some (aux query json)


    (** The build_query function generates the query to send to the server.
        For now it outputs a string but it should likely output json and a [variables] json object.*)

    let rec mk_query : type a. a query -> string =
      function query -> 
        let rec build_fields : type a. a query -> string list = function
          | Empty -> []
          | Kind {siblings; _} -> (kind.to_string)::(build_fields siblings)
          | Name {siblings; _} -> (name.to_string)::(build_fields siblings)
          | Setkind {siblings; arguments; subquery;_} ->
             (Format.asprintf "%s {%s}"
                ((set_kind()).to_string arguments.arg_kind arguments.arg_ignore) (** arguments are in the same order as they are declared *)
                (mk_query subquery))::(build_fields siblings)
        in
        Stdlib.String.concat " " @@ build_fields query
  end

