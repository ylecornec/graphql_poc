type t = {
    id: int;
    name: string;
    address: Address.t;
  }

let dummy1 = {
    id = 0;
    name = "contact1";
    address = Address.dummy1;
  }

let dummy2 = {
    id = 1;
    name = "contact2";
    address = Address.dummy2;
  }

open Gql_types
open Graphql_lwt.Schema

module Gql =
  struct
    type query =
      [ `Address of Address.Gql.query |
        `Name |
        `Id ] list

    type response = 
      < id: Int.response;
      name: string option;
      address: Address.Gql.response > option
    type out = t option

    let address =
      Gql_fields.mk_field_zeroary 
        "address" ~typ:(Address.Gql.typ ()) ~resolve:(fun _ t -> Some t.address)

    let name =
      Gql_fields.mk_field_zeroary
        "name" ~typ:(Graphql_lwt.Schema.string) ~resolve:(fun _ t -> Some t.name)

    let id =
      Gql_fields.mk_field_zeroary
        "id" ~typ:(Graphql_lwt.Schema.int) ~resolve:(fun _ t -> Some t.id)

    let build_field = function
      | `Address q -> Format.asprintf "%s {%s}" (address.name) (Address.Gql.build_query q)
      | `Name -> name.name
      | `Id -> id.name

    let build_query (query_list: query) =
      Stdlib.String.concat " " (Stdlib.List.map build_field query_list)

    let typ (): (unit, out) typ = 
      obj "Contact"
        ~fields:(fun _ -> [
                     address.field ();
                     name.field ();
                     id.field ();
        ])

    let response_of_json (json: Yojson.Basic.t) : response =
      Some(
          object
            method address = Address.Gql.response_of_json (Json.get address.name json)
            method name = String.response_of_json (Json.get name.name json)
            method id = Int.response_of_json (Json.get id.name json)
          end
        )
  end

module Gql_non_nullable =
  Non_null (struct
      type new_out = t
      type new_response =
        < id: Int.response;
        name: String.response;
        address: Address.Gql.response >
    end) (Gql)

