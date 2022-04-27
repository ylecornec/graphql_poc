open Graphql_lwt
open Graphql_lwt.Schema

module Gql =
  struct
    type t = Lib.Contacts.t

    type src = unit

    type 'a res = {contacts: 'a}
    type _ query =
      Contacts: {subquery : 'sub Lib.Contacts.Gql.query} -> 'sub res query 

    type out = unit option

    let contacts =
      Lib.Gql_fields.field
        "contacts" ~args:[] ~typ:(Lib.Contacts.Gql.typ ()) ~resolve:(fun _ () -> Some Lib.Contacts.dummy)

    let typ () = non_null @@ obj "Query" ~fields:(fun _ -> [contacts.field ()])

    let mk_query = function
      | Contacts {subquery}  ->
         Format.sprintf "{%s { %s}}" (contacts.to_string) (Lib.Contact.Gql.mk_query subquery)

    let response_of_json query json =
      match query with
        Contacts {subquery} -> 
         { contacts = Lib.Contacts.Gql.response_of_json subquery (Lib.Json.get contacts.name json) }
  end

let schema =
  Schema.(
    schema
    [Gql.contacts.field ()]
  )
