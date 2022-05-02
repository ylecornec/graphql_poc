open Async
open Client_lib

(** The following file contains examples of query definitions and calls to the graphql server.

    We make use of the [query] type, which is built alongside the graphql schema.
    This ensures that we can only build valid queries and the server takes care of converting the argument to json when generating the query.
 *)

module Req = struct

  (** The query is built using a gadt, so that the return type of the [response_of_json] function below may be dependent on the query value.
      Once converted to a string the following query will be :
      {contacts {address {name kind}}}
   *)

  let query =
    let open Server_schema.Gql in
    Contacts {
        subquery = Address {
                       siblings = Empty;
                       subquery = Name {siblings = Kind {siblings = Empty}}}}

  (**
     The [response_of_json] below returns a record which type depend on the query.
     It might be convenient to define aliases to this type (with the help of merlin) to help bring the fields of this record into scope. See the [print_contact_name] function below.
   *)
  type contact = ((Lib.Kind.Gql.out, string, unit) Lib.Address.Gql.r option, unit, unit)
                   Lib.Contact.Gql.r
  type result = contact option list Server_schema.Gql.res

  let result json =
    Server_schema.Gql.response_of_json query json

end

module Req_setkind = struct
  let query =
    let open Server_schema.Gql in
    Contacts {
        subquery = Address {
                       siblings = Empty;
                       subquery = Setkind {
                                      siblings = Empty;
                                      arguments =
                                        {arg_kind = Lib.Kind.Kind2; arg_ignore = false} ;
                                      subquery = Name {siblings = Kind {siblings = Empty}}}}}


  type contact = ((unit, unit, (Lib.Kind.Gql.out, string, unit) Lib.Address.Gql.r option)
                    Lib.Address.Gql.r option,
                  unit, unit) Lib.Contact.Gql.r

let result= 
  function json ->
    let () = Format.printf "parsing json =\n    %s\n" (Yojson.Basic.to_string json) in
    Server_schema.Gql.response_of_json query json

end

let [@warning "-8"] req_setkind () =
  let%map res =
    Client.query_exn
      (object
         method query =
           let query_string = Server_schema.Gql.mk_query (Req_setkind.query) in
           let () = Format.printf "generated_query = %s \n" query_string in
           query_string
         method variables = `Assoc []
         method parse r = Req_setkind.result r
       end)
      (Uri.of_string "http://localhost:8080/graphql")
  in
  Stdlib.List.iter
    (function Some (contact: Req_setkind.contact) ->
               let Some address = contact.res_address in
               let Some address = address.res_setkind in
               Format.printf "name=%s,kind=%a\n" address.res_name (Format.pp_print_option Lib.Kind.pp) address.res_kind
    )
    res.contacts

let [@warning "-8-27"] req () =
  let%map res =
    Client.query_exn
      (object
         method query =
           let query_string = Server_schema.Gql.mk_query (Req.query) in
           let () = Format.printf "generated_query = %s \n" query_string in
           query_string
         method variables = `Assoc []
         method parse r = Req.result r
       end)
      (Uri.of_string "http://localhost:8080/graphql")
  in
  let print_contact_name (contact:Req.contact) = 
      let Some address = contact.res_address in
      let name = address.res_name in
      Format.printf "name=%s, kind=%a\n" name (Format.pp_print_option Lib.Kind.pp) address.res_kind
  in
  Stdlib.List.iter
    (function Some contact -> print_contact_name contact)
    res.contacts

let main = 
  let%map () = req () in
  let%map () = req_setkind () in
  shutdown 0

open Core
let () = never_returns (Scheduler.go ())
