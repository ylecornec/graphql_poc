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

open Graphql_lwt.Schema

module Gql = struct
  type ('address, 'name, 'id) r =
    {
      res_address: 'address;
      res_name: 'name;
      res_id: 'id;
    }

  type 'a res = 'a option

  type _ query =
    Empty: (unit, unit, unit) r query
  | Address :
      {siblings: (unit, 'name, 'id) r query;
       subquery: 'a Address.Gql.query;
      } -> 
      ('a Address.Gql.res, 'name, 'id) r query
  | Name:
      {siblings : ('address, unit, 'id) r query;} ->
      ('address, string, 'id) r query

  | Id: {siblings: ('address, 'name, unit) r query}  ->
        ('address, 'name, int) r query


  type out = t option

  let address =
    Gql_fields.field "address"
      ~args:[] ~typ:(Address.Gql.typ ()) ~resolve:(fun _ t -> Some t.address)

  let name =
    Gql_fields.field "name"
      ~args:[] ~typ:(Graphql_lwt.Schema.string) ~resolve:(fun _ t -> Some t.name)

  let id =
    Gql_fields.field
      "id" ~args:[] ~typ:(Graphql_lwt.Schema.int) ~resolve:(fun _ t -> Some t.id)

  let response_of_json: type a. a query -> Yojson.Basic.t -> a res =
    fun query json ->
    let rec aux: type a.a query -> Yojson.Basic.t -> a
      = fun query json ->
      match query with
      | Empty -> {res_address = (); res_id =  (); res_name = ();}
      | Name {siblings} ->
         { (aux siblings json) with res_name = Json.get_string @@ Json.get name.name json }
      | Id {siblings} ->
         { (aux siblings json) with res_id = Json.get_int @@ Json.get id.name json }
      | Address {siblings; subquery} ->
         { (aux siblings json) with res_address = Address.Gql.response_of_json subquery @@ Json.get address.name json }
    in
    match json with
    | `Null -> None 
    | json -> Some (aux query json)

  let rec fields_to_string : type a. a query -> string list = function
    | Empty -> []
    | Address {siblings; subquery} ->
       (Format.sprintf "%s {%s}" address.to_string (Address.Gql.mk_query subquery))::(fields_to_string siblings)
    | Name {siblings; _} -> name.to_string::(fields_to_string siblings)
    | Id {siblings; _} -> id.to_string::(fields_to_string siblings)

  let mk_query q = Stdlib.String.concat " " (fields_to_string q)

  let typ (): (unit, out) typ = 
    obj "Contact"
      ~fields:(fun _ -> [
                   address.field ();
                   name.field ();
                   id.field ();
      ])

end
