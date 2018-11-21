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

let bnot e =
    match e with
    | ValBool b -> ValBool (not b)
    | _ -> raise (BuiltinException "!: type error - not a boolean")


let rec beqop e1 e2 =
    let res =
        match e1, e2 with
        | ValNone, ValNone -> true
        | ValBool a, ValBool b -> a = b
        | ValInt a, ValInt b -> a = b
        | ValString s1, ValString s2 -> s1 = s2
        | ValFunc (n1, b1), ValFunc (n2, b2) -> (n1 = n2) && (b1 = b2)
        | ValList l1, ValList l2 -> 
            List.fold (&&) true 
                (List.zip l1 l2 |> List.map (fun (x, y) -> 
                    match beqop x y with
                    | ValBool inner -> inner
                    | _ -> false)
                )
        | ValReference r1, ValReference r2 -> r1 = r2
        | _, _ -> 
            raise (BuiltinException "==: type error on one or more arguments")
    ValBool res

let bcompop op numOp e1 e2 =
    match e1, e2 with
    | ValInt a, ValInt b -> ValBool (numOp a b)
    | _, _ -> 
        raise (BuiltinException 
            (sprintf "%s: type error on one or more arguments" 
                (optostr op)))

let bnumop op numOp e1 e2 =
    match e1, e2 with
    | ValInt a, ValInt b -> ValInt (numOp a b)
    | _, _ -> 
        raise (BuiltinException 
            (sprintf "%s: type error on one or more arguments" 
                (optostr op)))

let opto op =
    match op with
    | Add -> bnumop Add (+)
    | Sub -> bnumop Sub (-)
    | Mult -> bnumop Mult (*)
    | Div -> bnumop Div (/)
    | Eq -> beqop
    | Neq -> (fun a b -> bnot (beqop a b))
    | Leq -> bcompop Leq (<=)
    | Geq -> bcompop Geq (>=)
    | Lt -> bcompop Lt (<)
    | Gt -> bcompop Gt (>)

let bbinary bop =
    opto bop

let builtins : Map<string, Value> = 
    [
        ("print", ValBuiltinFunc bprint)
        ("hello", ValString "Hello, world!")
    ] |> Map.ofSeq