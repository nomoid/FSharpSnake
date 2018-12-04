module Builtins

open ProjectParser
open InterpreterTypes
open ScopeHelper

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

let twoarg name args =
    match args with
    | [x; y] -> x, y
    | _ -> raise (BuiltinException
            (sprintf "%s: can only be called with two arguments" name))

let anonfuncargs i =
    [0..(i-1)]
    |> List.map (sprintf "arg%i")
    |> String.concat ","

let rec bstropt inner args scope =
    match singlearg "tostring" args with
    | ValInt i -> ValString (sprintf "%i" i), scope
    | ValString s -> 
        if inner then
            ValString (sprintf "\"%s\"" s), scope
        else
            ValString (sprintf "%s" s), scope
    | ValBool b -> ValString (if b then "true" else "false"), scope
    | ValListReference ref ->
        let values = getListFromRef ref scope
        let rec bprintinner xs scope =
            match xs with
            | [] -> [], scope
            | v :: remaining ->
                let vOut, newScope = bstropt true [v] scope
                match vOut with
                | ValString s -> 
                    let rest, newScope2 = bprintinner remaining newScope
                    s :: rest, newScope2
                | _ -> raise (BuiltinException "Invalid return type for ToString")
        let outList, outScope = bprintinner values scope
        ValString (sprintf "[%s]" (String.concat ", " outList)), outScope
    | ValNone -> ValString "none", scope
    | ValReference xs ->
        match xs with
        | [] -> ValString ("emptyref"), scope
        | x :: _ ->
            match x with
            | ScopeGlobal -> ValString ("global"), scope
            | TempBlock -> ValString ("tempblock"), scope
            | FuncLocal (s, i) ->
                ValString (sprintf "local %s: %i" s i), scope
            | ScopeLocal s -> ValString (sprintf "type %s" s), scope
            | ScopeInstance (s, i) ->
                ValString (sprintf "instance %s: %i" s i), scope
    | ValFunc (args, _) ->
        ValString (sprintf "anonfunc(%s)" (anonfuncargs args.Length)), scope
    | ValBuiltinFunc _ -> ValString ("builtinfunc"), scope
    | ValListInner _ ->
        raise (BuiltinException "tostring: cannot directly access list inner")
    | ValOrderedNamespace _ ->
        raise (BuiltinException
            "tostring: cannot directly access ordered namespace")
    //| _ -> raise (BuiltinException "tostring: input type cannot be converted to string")

let bstr args scope =
    bstropt false args scope
let rec bprint args scope =
    let v, newScope = bstr args scope
    match v with
    | ValString s -> printfn "%s" s; ValNone, scope
    | _ -> raise (BuiltinException "Invalid return type for ToString")
    

let bsqrt args =
    match singlearg "sqrt" args with
    | ValInt i -> ValInt (int (sqrt (float i)))
    | _ -> raise (BuiltinException "sqrt: invalid input type")

let bnot e =
    match e with
    | ValBool b -> ValBool (not b)
    | _ -> raise (BuiltinException "!: type error - not a boolean")

let bpushf args scope =
    match twoarg "pushf" args with
    | ValListReference ref, v ->
        let xs = getListFromRef ref scope
        ValNone, setListToRef ref (v :: xs) scope
    | _ -> raise (BuiltinException "pushf: type error - first argument not a list")

let bpopf args scope =
    match singlearg "popf" args with
    | ValListReference ref ->
        let xs = getListFromRef ref scope
        if xs.IsEmpty then
            raise (BuiltinException "popf: cannot pop from empty list")
        else
            let v = List.head xs
            v, setListToRef ref (List.tail xs) scope
    | _ -> raise (BuiltinException "popf: type error - first argument not a list")

let bconcat args scope =
    match twoarg "concat" args with
    | ValListReference ref1, ValListReference ref2 ->
        let xs = getListFromRef ref1 scope
        let ys = getListFromRef ref2 scope
        makeNewList (xs @ ys) scope
    | ValString s1, ValString s2 ->
        ValString (s1 + s2), scope
    | _ -> raise (BuiltinException "concat: type error")

let blen args scope =
    match singlearg "len" args with
    | ValListReference ref ->
        let xs = getListFromRef ref scope
        ValInt xs.Length, scope
    | ValString s ->
        ValInt s.Length, scope
    | _ -> raise (BuiltinException "len: type error")

let rec brange args scope =
    match args with
    | [x] ->
        brange [ValInt 0; x] scope
    | [x; y] ->
        match x, y with
        | ValInt i, ValInt j ->
            if i >= j then
                makeNewList [] scope
            else
                makeNewList (List.map ValInt [i..(j-1)]) scope
        | _ ->
            raise (BuiltinException "range: type error - argument not an int")
    | _ -> raise (BuiltinException "range: incorrect number of arguments")

let bclone args scope =
    match singlearg "clone" args with
    | ValListReference ref ->
        let xs = getListFromRef ref scope
        makeNewList xs scope
    | ValInt i -> ValInt i, scope
    | ValString s -> ValString s, scope
    | ValBool b -> ValBool b, scope
    | _ ->
         raise (BuiltinException "clone: type error - unsupported type")

let rec bsublist args scope =
    let rec skipn n l =
        match l, n with
        | _, 0 -> l
        | [], _ -> raise (BuiltinException "sublist: index out of bounds")
        | _ :: xs, _ -> skipn (n - 1) xs
    match args with
    | [x; y] ->
        match x with
        | ValListReference ref ->
            let len, newScope = blen [ValListReference ref] scope
            bsublist [x; y; len] newScope
        | _ -> raise (BuiltinException "sublist: type error")
    | [x; y; z] ->
        match x, y, z with
        | ValListReference ref, ValInt i, ValInt j ->
            let xs = getListFromRef ref scope
            if j < i then
                raise (BuiltinException "sublist: index out of bounds")
            elif j > xs.Length then
                raise (BuiltinException "sublist: index out of bounds")
            else
                let ys = List.truncate (j - i) (skipn i xs)
                makeNewList ys scope
        | _ -> raise (BuiltinException "sublist: type error")
    | _ -> raise (BuiltinException "sublist: incorrect number of arguments")

let brand args =
    match singlearg "rand" args with
    | ValInt i ->
        let rnd = System.Random()
        ValInt (rnd.Next() % i)
    | _ -> raise (BuiltinException "rand: type error - argument not an int")


let rec beqop e1 e2 =
    let res =
        match e1, e2 with
        | ValNone, ValNone -> true
        | ValBool a, ValBool b -> a = b
        | ValInt a, ValInt b -> a = b
        | ValString s1, ValString s2 -> s1 = s2
        | ValFunc (n1, b1), ValFunc (n2, b2) -> (n1 = n2) && (b1 = b2)
        | ValListReference l1, ValListReference l2 ->
            //TODO stuctural equality for lists
            l1 = l2
            //List.fold (&&) true
            //    (List.zip l1 l2 |> List.map (fun (x, y) ->
            //        match beqop x y with
            //        | ValBool inner -> inner
            //        | _ -> false)
            //    )
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
        ("str", ValBuiltinFunc bstr)
        ("print", ValBuiltinFunc bprint)
        ("sqrt", ValBuiltinFunc (contextfree bsqrt))
        ("pushf", ValBuiltinFunc bpushf)
        ("popf", ValBuiltinFunc bpopf)
        ("concat", ValBuiltinFunc bconcat)
        ("len", ValBuiltinFunc blen)
        ("range", ValBuiltinFunc brange)
        ("clone", ValBuiltinFunc bclone)
        ("sublist", ValBuiltinFunc bsublist)
        ("rand", ValBuiltinFunc (contextfree brand))
        ("hello", ValString "Hello, world!")
    ] |> Map.ofSeq
