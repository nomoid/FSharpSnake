module Builtins

open ProjectParser
open InterpreterTypes

exception BuiltinException of string

let noarg name args =
    match args with
    | [] -> ()
    | _ -> raise (BuiltinException
            (sprintf "%s: can only be called with no arguments" name))

let singlearg name args =
    match args with
    | [x] -> x
    | _ -> raise (BuiltinException
            (sprintf "%s: can only be called with one argument" name))

let bprint args =
    match singlearg "Print" args with
    | ValInt i -> printfn "%i" i; ValNone
    | ValString s -> printfn "%s" s; ValNone
    | ValBool b -> printfn "%s" (if b then "true" else "false"); ValNone
    | _ -> raise (BuiltinException "Print: input type cannot be printed")

let bsqrt args =
    match singlearg "Sqrt" args with
    | ValInt i -> ValInt (int (sqrt (float i)))
    | _ -> raise (BuiltinException "Sqrt: invalid input type")

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

let bboolop op numOp e1 e2 =
    match e1, e2 with
    | ValBool a, ValBool b -> ValBool (numOp a b)
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

let bunaryminus e =
    match e with
    | ValInt v -> ValInt (-v)
    | _ -> raise (BuiltinException "-: type error on one or more arguments")

let bunaryplus e =
    match e with
    | ValInt v -> ValInt (v)
    | _ -> raise (BuiltinException "-: type error on one or more arguments")

let bunarynot e =
    match e with
    | ValBool v -> ValBool (not v)
    | _ -> raise (BuiltinException "-: type error on one or more arguments")

let opto op =
    match op with
    | Add -> bnumop Add (+)
    | Sub -> bnumop Sub (-)
    | Mult -> bnumop Mult (*)
    | Div -> bnumop Div (/)
    | Mod -> bnumop Mod (%)
    | Eq -> beqop
    | Neq -> (fun a b -> bnot (beqop a b))
    | Leq -> bcompop Leq (<=)
    | Geq -> bcompop Geq (>=)
    | Lt -> bcompop Lt (<)
    | Gt -> bcompop Gt (>)
    | And -> bboolop And (&&)
    | Or -> bboolop Or (||)
    | Dot -> raise (BuiltinException ". is not a binary op")

let bbinary bop =
    opto bop

let contextfree bfun =
    (fun vs scope -> bfun vs, scope)

let builtins : Map<string, Value> =
    [
        ("print", ValBuiltinFunc (contextfree bprint))
        ("sqrt", ValBuiltinFunc (contextfree bsqrt))
        ("hello", ValString "Hello, world!")
    ] |> Map.ofSeq
