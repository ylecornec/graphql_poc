type t = Contact.t list

let dummy = [Some Contact.dummy1; Some Contact.dummy2]

module Gql = (Gql_types.List(Contact.Gql))
