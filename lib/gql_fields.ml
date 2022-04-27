(**
   This file provides a wrapper around the graphql [field] object in
   order to additionaly build a [to_string] function used to serialize the field when generating a query.
   Instead of a string, this could be extented to build a [variables] json objects.
*)

open Graphql_lwt.Schema

(** informations to build a ocaml-graphql-server [arg] as well as a [to_string] function*)
type 'a arg = {
    name: string; (** the name of the argument*)
    typ: 'a Arg.arg_typ; (** the [typ] type from ocaml-graphql-server *)
    to_string: 'a -> string; (** a to string function that will be used when serializing a query *)
  }

module MyArg = struct
  type 'a my_arg
  type (_, _, _, _) myargs =
    | [] : ('ctx, 'out, 'out, string) myargs
    | ( :: ) : ('a arg) * ('ctx, 'out, 'args, 'args_string) myargs -> ('ctx, 'out, ('a -> 'args), ('a -> 'args_string)) myargs
end


(** build the ocaml-graphql-server [Arg.arg_list]*)
let rec args_of_myargs :
          type ctx out args args_string. (ctx, out, args, args_string) MyArg.myargs
               -> (out, args) Arg.arg_list
  = function
  | MyArg.[] -> Arg.[]
  | MyArg.(h::t) ->
     let graphql_arg = (Arg.arg h.name ~typ:h.typ) in
     Arg.(graphql_arg::(args_of_myargs t))

(** a record contraining the ocaml-graphql-server [field], its [name]
    and a [to_string] function to be used when serializing a query *)
type ('arg_type, 'args_to_string, 'out, 'subquery) field =
  {
    field: 'arg_type;
    to_string: 'args_to_string;
    name: string;
  }

let rec to_string :
          type ctx out arg args_string. string -> (ctx, out, arg, args_string) MyArg.myargs -> string list -> args_string = 
  fun name l acc ->
    match l with
    | MyArg.[] ->
       begin
         match acc with
         | [] -> name
         | _ -> Format.sprintf "%s(%s)" name (String.concat ", " acc)
       end
  | MyArg.(h::t) -> fun x -> (to_string name t (Format.asprintf "%s: %s" h.name (h.to_string x)::acc))

let [@warning "-27"] field name ~typ ~resolve ~(args:(_,'out,_,_) MyArg.myargs): (_, _,'out,_) field =
  let open Graphql_lwt.Schema in
  let to_string = to_string name args [] in
  let args = args_of_myargs args in
  let field = function () -> field name ~args:args ~typ ~resolve in
  {name; field; to_string}
