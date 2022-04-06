open Graphql_lwt
open Graphql_lwt.Schema

module Gql =
  struct
    type t = Lib.Contacts.t

    type src = unit

    type query = [ `Contacts of Lib.Contact.Gql.query ]
    type response = < contacts: Lib.Contacts.Gql.response >
    type out = unit option

    let contacts =
      Lib.Gql_fields.mk_field_zeroary
        "contacts" ~typ:(Lib.Contacts.Gql.typ ()) ~resolve:(fun _ () -> Some Lib.Contacts.dummy)

    let typ () = obj "Query" ~fields:(fun _ -> [contacts.field ()])

    let response_of_json (json: Yojson.Basic.t) =
      object
        method contacts = Lib.Contacts.Gql.response_of_json (Lib.Json.get contacts.name json)
      end

    let build_query = function
      | `Contacts q  ->
         Format.sprintf "{%s { %s}}" (contacts.to_string) (Lib.Contact.Gql.build_query q)
  end

let schema =
  Schema.(
    schema
    [Gql.contacts.field ()]
  )
