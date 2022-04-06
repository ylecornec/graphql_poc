type t = Contact.t list

let dummy = [Contact.dummy1; Contact.dummy2]

module Gql = (Gql_types.List(Contact.Gql_non_nullable))
