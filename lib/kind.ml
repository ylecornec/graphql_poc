type t = Kind1 | Kind2 | Kind3 [@@deriving show {with_path = false}, yojson]

let arg_typ () =
  let open Wrapper.Make(Graphql_lwt.Schema) in
  let open Arg in
  enum "Kind" ~values: [ enum_value "Kind1" ~value:Kind1;
                         enum_value "Kind2" ~value:Kind2]


module Schema = Wrapper.Make(Graphql_lwt.Schema)
module Gql_functors = Gql_types.Make(Graphql_lwt.Schema)
module Gql = Gql_functors.Make_non_null_scalar (
                 struct
                   type nonrec t = t
                   let typ_nullable () =
                     let open Schema in
                     enum "Kind" ~doc:"Kind"
                       ~values: [ enum_value "Kind1" ~value:Kind1; enum_value "Kind2" ~value:Kind2]

                   let response_of_json = function
                     | `String "Kind1" -> Kind1
                     | `String "Kind2" -> Kind2
                     | `String "Kind3" -> Kind3
                     | _ -> failwith "Kind.response_of_json failure"
                   
                 end
               )

(* module Gql : Gql_types.TYP = struct *)
(*   open Wrapper.Make(Graphql_lwt.Schema) *)
(*   type 'a res = 'a option *)
(*   type out_before_modifiers = t *)
(*   type 'a modifier = 'a *)
(*   type 'a final_option_modifier = 'a option *)


(*   type response = t option *)

(* end *)
