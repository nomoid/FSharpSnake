module Builtins

open ProjectParser

exception BuiltinException of string

let bprint args =
    match args with
    | [x] -> 
        match x with
        | ValInt i -> printfn "%i" i; ValNone
        | ValString s -> printfn "%s" s; ValNone
        | ValBool b -> printfn "%s" (if b then "true" else "false"); ValNone
        | _ -> raise (BuiltinException "Print: input type cannot be printed")
    | _ -> 
        raise (BuiltinException 
            "Print: can only be called with one argument")


let builtins : Map<string, Value> = 
    [
        ("print", ValBuiltinFunc bprint)
        ("hello", ValString "Hello, world!")
    ] |> Map.ofSeq