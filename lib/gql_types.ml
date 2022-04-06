open Graphql_lwt.Schema

(** A module providing the typ value used to build the graphql schema as well as
    well as utility functions to build queries and parse responses.
 *)
module type TYP = sig
  type out

  (** [typ] value for the graphql schema *)
  val typ: unit -> (unit, out) typ

 (** object type used to parse the response *)
  type response
  val response_of_json: Yojson.Basic.t -> response

 (** type used to build a query *)
  type query 
  val build_query : query -> string
end

module Int = struct
  type query = unit
  type out = int option
  type response = int option
  let typ () = int 
  let response_of_json = function
    | `Int i -> Some i
    | _ -> None
end

module String = struct
  type query = unit
  type out = string option
  type response = string option
  let typ () = string
  let response_of_json = function
    | `String i -> Some i
    | _ -> None
end

module Non_null (Aux: sig type new_out type new_response end)
         (Input: TYP with type out = Aux.new_out option and type response = Aux.new_response option)
  = struct
  type out = Aux.new_out
  type response = Aux.new_response
  type query = Input.query
  let typ () = non_null @@ Input.typ ()
  let response_of_json json =
    match Input.response_of_json json with
    | None -> failwith @@ "Non nullable value should not return None "^__LOC__
    | Some v -> v
  let build_query = Input.build_query
end

module List (Input: TYP)
  = struct
  type response = Input.response list
  type query = Input.query
  type out = Input.out list option
  let typ () = list @@ Input.typ ()
  let response_of_json json = 
    match json with
    | `List l -> List.map Input.response_of_json l
    | _ -> failwith @@ Format.asprintf "expecting a json list (%s): but got\n%a\n " __LOC__ Yojson.Basic.pp json
  let build_query = Input.build_query
end
