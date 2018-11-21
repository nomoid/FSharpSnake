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

let bnumop name numOp e1 e2 =
    match e1, e2 with
    | ValInt a, ValInt b -> ValInt (numOp a b)
    | _, _ -> 
        raise (BuiltinException 
            (sprintf "%s: type error on one or more arguments" name))

let badd = bnumop "Add" (+)
let bsub = bnumop "Subtract" (-)
let bmult = bnumop "Multiply" (*)
let bdiv = bnumop "Divide" (/)

let builtins : Map<string, Value> = 
    [
        ("print", ValBuiltinFunc bprint)
        ("hello", ValString "Hello, world!")
    ] |> Map.ofSeq