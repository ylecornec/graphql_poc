(**
   This file provides a wrapper around the graphql [field] object in
    order to ensures that we provide a [to_string] function of the
    corresponding type that can be used to serialize the arguments of
    the field.

    Instead of a string, this could be extented to build a [variables] json objects.
*)

open Graphql_lwt.Schema

(** This [myargs] type is used as the [arg_list] type from ocaml-graphql-server,
    but builds the type of a [to_string] function as well. *)

module MyArg = struct
  type 'a my_arg
  type (_, _, _, _) myargs =
    | [] : ('ctx, 'out, 'out, string) myargs
    | ( :: ) : ('a Arg.arg) * ('ctx, 'out, 'args, 'args_string) myargs -> ('ctx, 'out, ('a -> 'args), ('a -> 'args_string)) myargs
end

let rec args_of_myargs :
          type ctx out args args_string. (ctx, out, args, args_string) MyArg.myargs
               -> (out, args) Arg.arg_list
  = function
  | MyArg.[] -> Arg.[]
  | MyArg.(h::t) ->
     Arg.(h::(args_of_myargs t))

type ('arg_type, 'args_to_string) field =
  {
    field: 'arg_type;
    to_string: 'args_to_string;
    name: string;
  }

let mk_field name ~typ ~resolve ~(args:( _, _, _, 'to_string) MyArg.myargs) ~(to_string: 'to_string) =
  let open Graphql_lwt.Schema in
  let args = args_of_myargs args in
  let field = function () -> field name ~args:args ~typ ~resolve in
  {name; field; to_string}

let mk_field_zeroary name ~typ ~resolve =
  mk_field name ~typ ~resolve ~args:[] ~to_string:name
