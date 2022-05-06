(** test for the complex graphql argument types using obj *)
type t = {a: string; b: string; c:int} [@@deriving show]

open Wrapper.Make(Graphql_lwt.Schema)
 
let arg_typ () =
  let fields =
    Arg.[
        arg "a" ~typ:(non_null string);
        arg "b" ~typ:(non_null string);
        arg "c" ~typ:(non_null int);
    ]
  in
  Arg.obj "obj_arg"
    ~doc:"obj_arg doc"
    ~coerce:(fun a b c ->
      try
        Ok { a; b; c }
      with _ -> Error "Invalid format for obj_arg")
    ~fields
    ~to_string:(fun f {a;b;c} -> f a b c)
