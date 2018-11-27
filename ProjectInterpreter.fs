module ProjectInterpreter

open ProjectParser
open InterpreterTypes
open ScopeHelper
open Builtins

//Consts
let anonArgs = "$anon$"

let constructorName = "new"

let mainFunc = "main"

type EvalExpr =
    | Unevaled of Expr
    | Evaled of Value

type OrderedNamespace =
    | ONSSubspace of (string * OrderedNamespace) list
    | ONSVar of EvalExpr
    | ONSFunc of FuncType

type Namespace =
    | NSSubspace of Map<string, Namespace>
    | NSVar of EvalExpr
    | NSFunc of FuncType

type LValueStore =
    | PropertyStore of NoRefScope * string
    | ArrayStore of string * int
    | IdentifierStore of string

//let references : System.Collections.Generic.Dictionary<string, Namespace>
//    = new System.Collections.Generic.Dictionary<string, Namespace>()

let rec makeNamespace defns =
    match defns with
    | [] -> []
    | defn :: remaining ->
        let ending = makeNamespace remaining
        match defn with
        | FunctionDefn (name, args, body) ->
            (name, ONSFunc(args, body)) :: ending
        | ScopeDefn (name, body) ->
            (name, ONSSubspace (makeNamespace body)) :: ending
        | AssignmentDefn (name, expr) ->
            (name, ONSVar (Unevaled expr)) :: ending

//Name resolution & updating

//Note: for this specific function, the first returned scope is the scope of
//the resolved object, not the current scope
let resolveName name (scope : Scope) : (Value * Scope) * Scope =
    let _, refs = scope
    let rec resolveSubspaces (scope : Scope) : (Value * Scope) option =
        let nrs, refs = scope
        match nrs with
        | [] -> None
        | _ :: remaining ->
            match getFromMapping name scope with
            //Try finding in above scopes
            | None -> resolveSubspaces (remaining, refs)
            //Try finding in current scope
            | Some v -> Some (v, scope)
    let currResolveName = resolveSubspaces scope
    let resV, newScope =
        match currResolveName with
        | Some (v, newScope) -> v, newScope
        | None ->
            // Try finding in builtins
            match Map.tryFind name builtins with
            | Some v2 -> v2, ([], refs)
            | None ->
                raise (InterpreterException
                    (sprintf "Name \"%s\" cannot be resolved" name))
    (resV, newScope), scope

let updateName name newVal (scope : Scope) : Scope =
    let noRefScope, _ = scope
    match noRefScope with
    | [] -> raise (InterpreterException "Cannot update empty scope")
    | _ ->
        //Try finding existing value to update
        let rec updateSubspaces (scope : Scope) : Scope option =
            let nrs, refs = scope
            match nrs with
            | [] -> None
            | sname :: remaining ->
                match getFromMapping name scope with
                | None ->
                    match updateSubspaces (remaining, refs) with
                    | None -> None
                    | Some (innerScope, newRefs) ->
                        Some (sname :: innerScope, newRefs)
                | Some _ ->
                    Some (addToMapping name newVal scope)
                   
        let currResolveName = updateSubspaces scope     
        match currResolveName with
        | Some v -> v
        | None ->
            //Declare new local variable
            addToMapping name newVal scope

let rec makeArgsMap argNames args =
    match args with
    | [] ->
        match argNames with
        | [] -> Map.empty
        | _ -> raise (InterpreterException "Too few arguments provided")
    | arg :: remaining ->
        match argNames with
        | [] ->
            //TODO add anon args
            //let remainingMap = makeArgsMap [] remaining
            //match Map.tryFind anonArgs remainingMap with
            //| Some al ->
            //    match al with
            //    | ValListReference lr ->
            //        Map.add anonArgs (ValList (arg :: l)) remainingMap
            //    | _ -> raise (InterpreterException "Anon args is not a list")
            //| None ->
            //    Map.add anonArgs (ValList (arg :: List.empty)) remainingMap
            raise (InterpreterException "Too many arguments provided")
        | argName :: remainingNames ->
            let remainingMap = makeArgsMap remainingNames remaining
            if Map.containsKey argName remainingMap then
                raise (InterpreterException "Duplicate argument names")
            else
                Map.add argName arg remainingMap


let rec thisReference (scope : Scope) =
    let nrs, refs = scope
    match nrs with
    | [] -> raise (InterpreterException "Invalid context for \"this\"")
    | name :: remaining ->
        match name with
        | ScopeInstance _ -> nrs
        | _ -> thisReference (remaining, refs)

let rec setList l i v =
    match l, i with
    | [], _ -> raise (InterpreterException "Index out of bounds")
    | x :: xs, 0 -> v :: xs
    | x :: xs, n -> x :: (setList xs (n - 1) v)

//Program execution
let rec executeBlock block scope =
    //Make a new scope
    let newScope = pushTempScope scope
    //printfn "Push: %A" (fst newScope)
    //Eval statements
    let v, newScope2 = evalStmts block newScope
    let popped = popScope newScope2
    //printfn "Pop: %A" (fst newScope2)
    //printfn ""
    //Pop the new scope
    v, popped

and ifOnce cond block scope =
    let v, newScope = evalExpr cond scope
    match v with
    | ValBool b ->
        if b then
            let v, newScope2 = executeBlock block newScope
            Some v, newScope2
        else
            None, newScope
    | _ -> raise (InterpreterException "Condition must be of type bool")
and ifElse condBlockList optionBlock scope =
    match condBlockList with
    | [] ->
        match optionBlock with
        | Some s ->
            executeBlock s scope
        | None ->
            None, scope
    | (cond, block) :: remaining ->
        let ov, newScope = ifOnce cond block scope
        match ov with
        | Some s ->
            s, newScope
        | None ->
            ifElse remaining optionBlock newScope
and runWhile cond block scope =
    let v, newScope = ifOnce cond block scope
    match v with
    // Condition is false
    | None -> None, newScope
    | Some vin ->
        match vin with
        // Inner return
        | Some _ -> vin, newScope
        // Tail call while loop
        | None -> runWhile cond block newScope
and evalStmt stmt scope =
    //printfn "-----------------------------------------------------"
    //printfn "%A" stmt
    //printfn "------------------------"
    //printfn "%A" scope
    match stmt with
    | FunctionCallStmt(name, args) ->
        let argVals, newScope = evalArgs args scope
        let _, newScope2 = functionCallLocal name argVals newScope
        None, newScope2
    | AssignmentStmt(lv, oop, expr) ->
        let lvs, newScope0 = resolveLValue lv scope
        let vOuter, newScopeOuter = 
            match oop with
            | None -> evalExpr expr newScope0
            | Some op ->
                let v1, newScope1 = getLValue lvs newScope0
                let v2, newScope2 = evalExpr expr newScope1
                ((bbinary op) v1 v2), newScope2
        None, setLValue lvs vOuter newScopeOuter
        (*match lv with
        | Identifier name ->
            None, updateName name vOuter newScopeOuter
        | PropertyAccessor(_, name) ->
            match oref with
            | None -> 
                raise (InterpreterException "Cannot access None property")
            | Some vInner ->
                None, setProperty vInner name vOuter newScopeOuter
        | PropertyArrayAccessor(_, name, exprs) ->
            match oref with
            | None ->
                raise (InterpreterException "Cannot access None property")
            | Some vInner ->
                setArrayProperty vInner name vOuter
        | ArrayAccessor(expr, exprs) ->
            sprintf "%s%s"
                (prettyprintexpr expr)
                (prettyprintarrayaccess exprs)
        match oexpr, oop with
        | None, None ->
            None, updateName name v newScope
        | Some e, None ->
            let v1, newScope = evalExpr e scope
            let v2, newScope2 = evalExpr expr newScope
            None, setProperty v1 name v2 newScope2
        | None, Some op ->
            let ((v1, _), newScope) = resolveName name scope
            let v2, newScope2 = evalExpr expr newScope
            let v3, newScope3 = ((bbinary op) v1 v2), newScope2
            None, updateName name v3 newScope3
        | Some e, Some op ->
            let v1, newScope = evalExpr e scope
            let ((v2, _), newScope2) = accessProperty v1 name newScope
            let v3, newScope3 = evalExpr expr newScope2
            let v4, newScope4 = ((bbinary op) v2 v3), newScope3
            None, setProperty v1 name v4 newScope4*)
    | LetStmt(name, expr) ->
        let v, newScope = evalExpr expr scope
        None, addToMapping name v newScope
    | ReturnStmt expr ->
        let v, newScope = evalExpr expr scope
        Some v, newScope
    | IfElseStmt((cond, block), condBlockList, optionBlock) ->
        ifElse ((cond, block) :: condBlockList) optionBlock scope
    | WhileStmt(cond, block) ->
        runWhile cond block scope
and evalStmts stmts scope =
    //printfn "%A" (fst nsScope)
    match stmts with
    | [] -> None, scope
        //raise (InterpreterException "Function has no return value")
    | stmt :: remaining ->
        let evalStmtResult, newScope = evalStmt stmt scope
        match evalStmtResult with
        | Some v -> Some v, newScope
        | None -> evalStmts remaining newScope
and functionCall name (scopeInner : Scope) func args scope =
    //printfn "Push f: %A" (fst scope)
    let nrsOuter, _ = scope
    match func with
    | ValFunc (argNames, stmts) ->
        let argsMap = makeArgsMap argNames args
        //Push function local scope
        let newScope = pushFunctionLocalScope name argsMap scopeInner
        let ov, newScope2 = evalStmts stmts newScope
        match ov with
        | None -> raise (InterpreterException "Function has no return value")
        | Some v ->
            let _, refsNew = popScope newScope2
            //printfn "Pop f: %A" (fst scope)
            //Pop function local scope
            v, (nrsOuter, refsNew)
    | ValBuiltinFunc (f) ->
        //printfn "Pop f: %A" (fst scope)
        f args scope
    | _ -> raise (InterpreterException "Tried to call non-function object")
and functionCallLocal name args scope =
    let (func, scopeInner), scopeOuter = resolveName name scope
    functionCall name scopeInner func args scopeOuter
and functionCallProperty v name args scope =
    let ((func, innerScope), outerScope) = accessProperty v name scope
    functionCall name innerScope func args outerScope
and evalArgs args scope =
    match args with
    | [] -> [], scope
    | arg :: remaining ->
        let v, newScope = evalExpr arg scope
        let vals, finalScope = evalArgs remaining newScope
        v :: vals, finalScope
and accessPropertyFromRef ref name scope =
    let _, refs = scope
    let res = getFromMapping name (ref, refs)
    //let (a, _) = resolveName (xs, refs) name
    match res with
    | None -> 
        raise (InterpreterException
            (sprintf "Property \"%s\" not found" name))
    | Some v -> (v, (ref, refs)), scope
and accessProperty v name scope =
    match v with
    | ValReference xs ->
        accessPropertyFromRef xs name scope
    | _ -> 
        raise (InterpreterException
            "Tried accessing a property of an object without properties")
and setProperty v1 name v2 scope =
    let nrs, refs = scope
    match v1 with
    | ValReference xs ->
        let _, newRefs = addToMapping name v2 (xs, refs)
        nrs, newRefs
    | _ ->
        raise (InterpreterException
            "Tried setting a property of an object without properties")
and resolveProperty v name scope =
    match v with
    | ValReference xs ->
        PropertyStore(xs, name)
    | _ ->
        raise (InterpreterException
            "Object does not have properties")
and accessArrayFromRef ref i scope =
    let l = getListFromRef ref scope
    if i < 0 || i >= l.Length then
        raise (InterpreterException
            (sprintf "List index %i out of bounds" i))
    l.[i], scope
and setArrayFromRef ref i v scope =
    let l = getListFromRef ref scope
    if i < 0 || i >= l.Length then
        raise (InterpreterException
            (sprintf "List index %i out of bounds" i))
    setListToRef ref (setList l i v) scope
and accessArraySingle v1 v2 scope =
    match v1, v2 with
    | ValListReference ref, ValInt i ->
        accessArrayFromRef ref i scope
    | ValReference _, ValString s ->
        let ((v, _), newScope) = accessProperty v1 s scope
        v, newScope
    //| ValString s, ValInt i ->
    //    let ca = s.ToCharArray()
    //    if i < 0 || i > ca.Length then
    //        raise (InterpreterException
    //            (sprintf "String index %i out of bounds" i))
    //    ValString (System.String [|ca.[i]|]), scope
    | _ ->
        raise (InterpreterException
            "Invalid types for array access")
//and accessArray v argVals scope =
//    match argVals with
//    | [] ->
//        raise (InterpreterException
//            "Tried array access without arguments")
//    | [x] ->
//        let newV, newScope = accessArraySingle v x scope
//        (newV, v), newScope
//    | x :: remaining ->
//        let vOut, newScope = accessArraySingle v x scope
//        accessArray vOut remaining newScope
and resolveArray v argVals scope =
    //let v, newScope1 = evalExpr expr scope
    //let argVals, newScope2 = evalArgs args newScope1
    match argVals with
    | [] ->
        raise (InterpreterException
            "Tried array access without arguments")
    | [x] ->
        match v, x with
        | ValListReference ref, ValInt i ->
            ArrayStore(ref, i), scope
        | ValReference ref, ValString s ->
            PropertyStore(ref, s), scope
        | _ ->
            raise (InterpreterException
                "Invalid types for array access")
    | x :: remaining ->
        let vOut, newScope = accessArraySingle v x scope
        resolveArray vOut remaining newScope
and getLValue lvs scope =
    match lvs with
    | ArrayStore(ref, i) ->
        accessArrayFromRef ref i scope
    | PropertyStore(nrs, name) ->
        let ((v, _), newScope) = accessPropertyFromRef nrs name scope
        v, newScope
    | IdentifierStore name ->
        let ((v, _), newScope) = resolveName name scope
        v, newScope
and setLValue lvs v scope =
    let outerNrs, refs = scope
    match lvs with
    | ArrayStore(ref, i) ->
        setArrayFromRef ref i v scope
    | PropertyStore(nrs, name) ->
        let _, newRefs = addToMapping name v (nrs, refs)
        outerNrs, newRefs
    | IdentifierStore name ->
        updateName name v scope
and resolveLValue lvalue scope : (LValueStore * Scope) =
    match lvalue with
    | Identifier name ->
        IdentifierStore name, scope
    | PropertyAccessor(expr, name) ->
        let v1, newScope1 = evalExpr expr scope
        resolveProperty v1 name newScope1, newScope1
    | PropertyArrayAccessor(expr, name, args) ->
        let v1, newScope1 = evalExpr expr scope
        let ((v2, _), newScope2) = accessProperty v1 name newScope1
        let argVals, newScope3 = evalArgs args newScope2
        resolveArray v2 argVals newScope3
    | ArrayAccessor(expr, args) ->
        let v, newScope1 = evalExpr expr scope
        let argVals, newScope2 = evalArgs args newScope1
        resolveArray v argVals newScope2
and evalLValue lvalue scope : (Value * Scope) =
    let lvs, newScope = resolveLValue lvalue scope
    getLValue lvs newScope
and evalExpr expr scope : (Value * Scope) =
    match expr with
    | FunctionCallExpr (name, args) ->
        let argVals, newScope = evalArgs args scope
        functionCallLocal name argVals newScope
    | NumLiteral n -> ValInt n, scope
    | StringLiteral str -> ValString str, scope
    | BoolLiteral b -> ValBool b, scope
    | ListLiteral exprs ->
        let argVals, newScope = evalArgs exprs scope
        let c = getListCount newScope
        let newScope2 = setListCount (c + 1) newScope
        let ref = globalListName c
        ValListReference ref, setListToRef ref argVals newScope2
    | ThisLiteral -> ValReference (thisReference scope), scope
    | ParensExpr e -> evalExpr e scope
    | BinaryExpr(op, e1, e2) -> evalInfix (bbinary op) e1 e2 scope
    | UnaryMinus e -> evalWithFunc e bunaryminus scope
    | UnaryPlus e -> evalWithFunc e bunaryplus scope
    | UnaryNot e -> evalWithFunc e bunarynot scope
    | PropertyFunctionCall(expr, name, args) ->
        let v, newScope1 = evalExpr expr scope
        let argVals, newScope2 = evalArgs args newScope1
        functionCallProperty v name argVals newScope2
    | LValueExpr lv -> evalLValue lv scope
and evalWithFunc e func scope =
    let v, newScope = evalExpr e scope
    func v, newScope
and evalInfix processor e1 e2 scope =
    let vLeft, newScope1 = evalExpr e1 scope
    let vRight, newScope2 = evalExpr e2 newScope1
    processor vLeft vRight, newScope2

//let rec convertToNamespace (ns : (string * OrderedNamespace) list) =
//    match ns with
//    | [] -> Map.empty
//    | pair :: remaining ->
//        let map = convertToNamespace remaining
//        let name, ons = pair
//        let ns =
//            match ons with
//            | ONSVar v -> NSVar v
//            | ONSFunc(args, body) -> NSFunc(args, body)
//            | ONSSubspace list -> NSSubspace (convertToNamespace list)
//        Map.add name ns map

let staticScope scope =
    let nrs, refs = scope
    let rec staticScopeInner nrs =
        match nrs with
        | [] -> []
        | layer :: remaining ->
            let newLayer =
                match layer with
                | ScopeInstance (name, _) -> ScopeLocal name
                | _ -> layer
            let inner = staticScopeInner remaining
            newLayer :: inner
    staticScopeInner nrs, refs

let newInstance name args scope : Value * Scope =
    noarg "new" args
    //Pop until out of \"new\" scope
    let ((v, innerScope), newScope) = resolveName name scope
    //Make the scope static so a copy can be obtained
    let scopeToCopy = ScopeLocal name :: fst (staticScope innerScope)
    let count = getLocalCount (scopeLocalCountName name) innerScope
    let newInnerScope = setLocalCount (scopeLocalCountName name) (count + 1) innerScope
    let instanceName = ScopeInstance (name, count)
    let instanceScope = instanceName :: fst newInnerScope
    let newRefs =
        Map.add
            instanceScope
            (findRef (scopeToCopy, snd newInnerScope))
            (snd newInnerScope)
    //printfn "%A" (fst newScope, newRefs)
    //printfn ""
    let newScope2 = fst newScope, newRefs
    ValReference instanceScope, newScope2

let defaultConstructor name =
    ValBuiltinFunc(newInstance name)

let rec evalGlobals (ns : (string * OrderedNamespace) list) =
    //let firstPass = convertToNamespace ns
    let rec evalGlobalInner ns scope =
        match ns with
        | [] -> Map.empty, scope
        | pair :: remaining ->
            let name, ons = pair
            let ns, newScope =
                match ons with
                | ONSVar v ->
                    match v with
                    | Unevaled expr ->
                        let newVal, sc = evalExpr expr scope
                        NSVar (Evaled newVal), addToMapping name newVal sc
                    | Evaled _ ->
                        raise (InterpreterException
                            "Expr shound not be already evaluated")
                | ONSFunc(args, body) ->
                    NSFunc(args, body),
                        addToMapping name (ValFunc(args, body)) scope
                | ONSSubspace list ->
                    // Push and pop a new scope
                    let newVal, sc =
                        evalGlobalInner list
                            (pushScope
                                (ScopeLocal name)
                                (PersistentScope :: List.empty)
                                Map.empty scope
                            )
                    NSSubspace (newVal), popScope sc
            let map, newScope2 = evalGlobalInner remaining newScope
            let newScope3 =
                match ons with
                | ONSSubspace _ ->
                    let tempScope = addToMapping name (defaultConstructor name) newScope2
                    // Implementation of .new syntax
                    //let outerContext = fst newScope2
                    //let tempScope = addToMapping name (ValReference outerContext) newScope2
                    //let defConsMap = Map.add constructorName (defaultConstructor name) Map.empty
                    //let tempScope2 =
                    //    pushScope
                    //        name
                    //        (PersistentScope :: List.empty)
                    //        defConsMap
                    //        tempScope
                    //popScope tempScope2
                    tempScope
                | _-> newScope2
            Map.add name ns map, newScope3
    evalGlobalInner ns (globalScope,
        Map.add
            globalScope
            ((PersistentScope :: List.empty), Map.empty)
            Map.empty)


let runMain defns =
    let ons = makeNamespace defns
    let ns, scope = evalGlobals ons
    //printfn "%A" ns
    //printfn ""
    //printfn "%A" scope
    //printfn ""
    match Map.tryFind mainFunc ns with
    | Some v ->
        let res, _ = functionCallLocal mainFunc [] scope
        res
    | None -> raise (InterpreterException "No main function found")
       
