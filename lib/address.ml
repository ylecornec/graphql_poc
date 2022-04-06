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
    type out = t option

    (**
       The type used to build queries, with possible arguments in a type safe way.

       `Kind, `Name, and `SetKind correspond to fields from the [typ] object.

       However `GetAll correspond to a more complex query [GetAll.q] that we can parse the result of into a [GetAll.response] structure.
       In practice we could use the [fields_derivers] mechanism to build the [GetAll] module for this. 
     *)
    type query =
      [ `Kind
      | `Name
      | `Setkind of ( Kind.t * query)
      | `GetAll
      ] list

    (** The GetAll module would make use the fields_derivers mechanism *)
    module GetAll = struct
      let q = [`Kind; `Name]
      type response = {kind: Kind.t option; name : string option}[@@deriving show]
      let of_json (json: Yojson.Basic.t) =
        {
          kind = Kind.Typ.response_of_json (Json.get "kind" json);
          name = Gql_types.String.response_of_json (Json.get "name" json);
        }
    end 

    type response =
      < kind : Kind.Typ.response;
      name: string option;
      set_kind: response;
      get_all: GetAll.response;
      >


    let kind =
      Gql_fields.mk_field_zeroary "kind" ~typ:(Kind.Typ.typ) ~resolve:(fun _ t -> Some t.kind)

    let name =
      Gql_fields.mk_field_zeroary "name" ~typ:string ~resolve:(fun _ t -> Some t.name)

    (** I dont know if this happens in the codebase but in this
        example, I tried mutual recursion between the graphql [set_kind] field and [typ] object typ.
     *)
    let rec set_kind () =
      (** This list type from [Gql_fields.MyArg] is used as the [arg_list] type from ocaml-graphql-server,
          but ensures that we provide a [to_string] function for the right type as well. *)
      let args =
        Gql_fields.MyArg.[
            Arg.arg "kind" ~typ: (Arg.non_null @@ Kind.Typ.arg_typ ());
        ]
      in
      let to_string a = Format.asprintf "setKind(kind:%s)" (Kind.show a) in
      Gql_fields.mk_field "setKind"
        ~typ:(typ (): (_, t option) typ)
        ~resolve:(fun _ (t:t) new_kind -> t.kind <- new_kind; Some t)
        ~args
        ~to_string

    and typ () = 
      obj "Address"
        ~fields:(fun _ -> [
                     kind.field ();
                     (set_kind()).field ();
                     name.field ();
        ])


    (** The object type to parse the response *)
    let rec response_of_json json = object
       method kind = Kind.Typ.response_of_json (Json.get kind.name json)
       method set_kind = response_of_json (Json.get ((set_kind ()).name) json)
       method name = Gql_types.String.response_of_json (Json.get name.name json)
       method get_all = GetAll.of_json json
      end


    (** The build_query function generates the query to send to the server.
        For now it outputs a string but it should likely output json and a [variables] json object.*)

    let rec build_field = function
      | `Kind -> kind.to_string
      | `Name -> name.to_string
      | `GetAll -> build_query GetAll.q
      | `Setkind (arg, q) -> Format.asprintf "%s {%s}" ((set_kind()).to_string arg) (build_query q)

    and build_query (query_list: query) = String.concat " " (List.map build_field query_list)
  end
