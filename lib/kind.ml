type t = Kind1 | Kind2 [@@deriving show {with_path = false}, yojson]

module Gql = struct
  open Wrapper.Make(Graphql_lwt.Schema)
  type out = t option
  type 'a res = 'a option

  let typ (): (unit, _) typ =
    enum "Kind" ~doc:"Kind"
      ~values: [ enum_value "Kind1" ~value:Kind1; enum_value "Kind2" ~value:Kind2]

  let arg_typ () =
    Arg.enum "Kind"
      ~values: [ enum_value "Kind1" ~value:Kind1; enum_value "Kind2" ~value:Kind2]
      ~to_string: show

  type response = t option

  let response_of_json (json:Yojson.Basic.t) =
    match json with
    | `Null -> None
    | json -> Some (match json with
                    | `String "Kind1" -> Kind1
                    | `String "Kind2" -> Kind2
                    | _ -> failwith "Kind.of_json failure")
end
