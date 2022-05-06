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

    type 'a res = 'a option
    type out = t res


    open Gql_types

    type ('kind, 'name, 'setkind) r =
      {
       res_kind: 'kind;
       res_name: 'name;
       res_setkind: 'setkind;
      }

    type setkind_args =
      {arg_kind: Kind.t;
       arg_ignore: bool;
       arg_obj: Obj_arg.t;
      }

    (**
       The type used to build queries, with possible arguments in a type safe way.
       `Kind, `Name, and `SetKind correspond to fields from the [typ] object.
     *)

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

    let kind : (_,_ , string, Kind.Gql.out, no_subquery) field =
      field "kind" ~args:[] ~typ:(Kind.Gql.typ()) ~resolve:(fun _ t -> Some t.kind)

    let name : (_,_, string, string, no_subquery) field =
      field "name" ~args:[] ~typ:(non_null string) ~resolve:(fun _ (t:t) -> t.name)

    (** There can be mutual recursion between the graphql [set_kind] field and [typ] object typ.*)

    let rec set_kind () : (unit, t, Kind.t -> bool -> Obj_arg.t -> string, t res, 'a) field =
      let args =
        Arg.[
            arg "kind" ~typ:(non_null @@ Kind.Gql.arg_typ ());
            arg "ignore" ~typ:(non_null bool);
            arg "obj_arg" ~typ:(Obj_arg.arg_typ());
       ]
      in
      field "setKind"
        ~typ:(typ (): (_, t option) typ)
        ~resolve: (fun _ (t:t) new_kind _ignore obj_arg ->
          let () =
            match obj_arg with
            | Some (Ok obj_arg) -> Format.printf "resolving setking obj_arg = %a\n" Obj_arg.pp obj_arg
            | _ -> Format.printf "obj_arg failure\n"
          in
          let () = Format.print_flush () in
          let () = if not _ignore then t.kind <- new_kind in
          Some t
        )
        ~args

    and typ () = 
      obj "Address" ~fields: (fun _ -> Fields.(([kind; set_kind (); name]: _ fields)))

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

    (** The mk_query function generates the query to send to the server.
        For now it outputs a string but it should likely output json and a [variables] json object.*)

    let rec mk_query : type a. a query -> string =
      function query -> 
        let rec build_fields : type a. a query -> string list = function
          | Empty -> []
          | Kind {siblings; _} -> kind.to_string::(build_fields siblings)
          | Name {siblings; _} -> name.to_string::(build_fields siblings)
          | Setkind {siblings; arguments; subquery;_} ->
             (Format.asprintf "%s {%s}"
                ((set_kind()).to_string arguments.arg_kind arguments.arg_ignore arguments.arg_obj) (** arguments are in the same order as they are declared *)
                (mk_query subquery))::(build_fields siblings)
        in
        Stdlib.String.concat " " @@ build_fields query
  end

