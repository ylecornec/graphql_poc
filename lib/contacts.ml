type t = Contact.t list

let dummy = [Some Contact.dummy1; Some Contact.dummy2]


open Gql_types.Make(Graphql_lwt.Schema)
module Gql = Nullable(List(Contact.Gql))
