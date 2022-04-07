open Core
open Async
open Client_lib


(**

   The following file contains examples of query definitions and calls to the graphql server.

   The [Req_set_kind] module builds a query using only "low-level" constructors that map directly to the graphql query.
   In that case, the server helps us parse the response but we must be carefull not to access fields we did not request.

   The [Req_get_all] module uses the [`GetAll] constructor that represent a more complex graphql query that was defined server side.
   Since the server knows about the fields of this query, we can direclty parse the result into a struct of the right type,
   which is more convenient and type safe.

  *)

module Req_set_kind = struct
  (**

     We make use of the [query] type, which is built alongside the graphql schema.
     This ensures that we can only build valid queries and the server takes care of converting the argument to json when generating the query.
     I used polymorphic variants for convenience to avoid scopping problems.
   *)

  let query = `Contacts
                 [`Address
                    [`Setkind (Lib.Kind.Kind1,
                               [`Kind; `Name])]]


  (**
     The server provides a [response_of_json] function, which returns an object that can be used to parse the response.

     Ideally, the type of this object would be dependent on the query value,
     so that we know which fields were requested by the client, but I did not find a suitable solution yet.

     So at the moment we need to be carefull to only access fields we asked for
     (but if the field is there it will be parsed correctly).

   *)

  type result_address = {name: string option; kind: Lib.Kind.t option}
  type result = result_address list

  let result json: result =
    List.map ~f:(fun o ->
        {name = o#address#set_kind#name;
         kind = o#address#set_kind#kind}
      )
      ((Server_schema.Gql.response_of_json json)#contacts)

end

module Req_get_all = struct
  (**
     This query makes use of the [`GetAll] constructor, which corresponds to a more complex subquery defined server side,
     which enables us to parse the response of this subqueries directly into a struct of type [Lib.Address.Gql.GetAll.response]
   *)

  let query = `Contacts [`Address [`GetAll]]

  type result = Lib.Address.Gql.GetAll.response list

  let result json : result =
    List.map
      ~f:(fun contact -> contact#address#get_all)
      (Server_schema.Gql.response_of_json json)#contacts

end

(** Below, we evaluate the queries and print the responses *)

let req_get_all () =
  let%map response =
    Client.query_exn
      (object
         method query = Server_schema.Gql.build_query Req_get_all.query
         method variables = `Assoc []
         method parse r = Req_get_all.result r
       end)
      (Uri.of_string "http://localhost:8080/graphql")
  in
  List.iter
    response
    ~f:(fun contact -> Format.printf "%a\n" (Lib.Address.Gql.GetAll.pp_response) contact)

let req_set_kind () = 
  let%map response =
    Client.query_exn
      (object
         method query = Server_schema.Gql.build_query Req_set_kind.query
         method variables = `Assoc []
         method parse r = Req_set_kind.result r
       end)
      (Uri.of_string "http://localhost:8080/graphql") 
  in
  List.iter
    response
    ~f:(fun {name; _} -> Format.printf "%a\n" (Format.pp_print_option Format.pp_print_string) name)

let main = 
  let%map () = req_set_kind () in
  let%map () = req_get_all () in
  shutdown 0

let () = never_returns (Scheduler.go ())
