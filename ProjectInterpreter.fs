module ProjectInterpreter

open ProjectParser
open Builtins

//Consts
let anonArgs = "$anon$"
let tempBlock = "$tempblock$"
let globalScopeName = "$scopeglobal$"

let blockPrefix = "" //TODO set block prefix
let blockName name =
    sprintf "$block_%s$" name

let scopeLocalName name =
    sprintf "$scopelocal_%s$" name

let functionLocalName name =
    sprintf "$funclocal_%s$" name

let mainFunc = "main"

type ScopeRules =
    | PersistentScope
type FuncType = string list * Stmt list
type RefType = ScopeRules list * Map<string, Value>
type NoRefScope = string list
type Refs = Map<NoRefScope, RefType>
type Scope = NoRefScope * Refs

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

//let references : System.Collections.Generic.Dictionary<string, Namespace>
//    = new System.Collections.Generic.Dictionary<string, Namespace>()

exception InterpreterException of string

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
let findRef (scope : Scope) : RefType =
    let nrs, refs = scope
    match Map.tryFind nrs refs with
    | None -> raise (InterpreterException "Scope not found in refs")
    | Some (opts, mapping) -> opts, mapping

//Scope helper methods
let popScope (scope : Scope) : Scope =
    //Leaving a scope
    let nrs, refs = scope
    let opts, _ = findRef scope
    if List.contains PersistentScope opts then
        List.tail nrs, refs
    else
        List.tail nrs, Map.remove nrs refs

let pushScope name rules defaultMapping (scope : Scope) : Scope =
    let nrs, refs = scope
    let newLocation = name :: nrs
    newLocation, Map.add newLocation (rules, defaultMapping) refs

let pushTempScope (scope : Scope) : Scope =
    pushScope tempBlock List.empty Map.empty scope

let pushFunctionLocalScope scopeName args (scope : Scope) : Scope =
    pushScope (functionLocalName scopeName) List.empty args scope

let addToMapping name value (scope : Scope) : Scope =
    let nrs, refs = scope
    let opts, mapping = findRef scope
    nrs, Map.add nrs (opts, Map.add name value mapping) refs

let getFromMapping name (scope : Scope) : Value option =
    let _, mapping = findRef scope
    Map.tryFind name mapping

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
    | [] -> Map.empty
    | arg :: remaining ->
        match argNames with
        | [] ->
            let remainingMap = makeArgsMap [] remaining
            match Map.tryFind anonArgs remainingMap with
            | Some al ->
                match al with
                | ValList l ->
                    Map.add anonArgs (ValList (arg :: l)) remainingMap
                | _ -> raise (InterpreterException "Anon args is not a list")
            | None ->
                Map.add anonArgs (ValList (arg :: List.empty)) remainingMap
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
        if name.StartsWith(blockPrefix) then
            nrs
        else
            thisReference (remaining, refs)

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
    match stmt with
    | FunctionCallStmt(name, args) ->
        let argVals, newScope = evalArgs args scope
        let _, newScope2 = functionCallLocal name argVals newScope
        None, newScope2
    | AssignmentStmt(name, expr) ->
        let v, newScope = evalExpr expr scope
        None, updateName name v newScope
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
        f args, scope
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
and accessProperty v name scope =
    let _, refs = scope
    match v with
    | ValReference xs ->
        let res = getFromMapping name (xs, refs)
        //let (a, _) = resolveName (xs, refs) name
        match res with
        | None -> 
            raise (InterpreterException
                (sprintf "Property \"%s\" not found" name))
        | Some v -> (v, (xs, refs)), scope
    | _ -> 
        raise (InterpreterException 
            "Tried accessing a property of an object without properties")

and evalExpr expr scope : (Value * Scope) =
    match expr with
    | FunctionCallExpr (name, args) ->
        let argVals, newScope = evalArgs args scope
        functionCallLocal name argVals newScope
    | Identifier name ->
        let ((v, _), newScope) = resolveName name scope
        v, newScope
    | NumLiteral n -> ValInt n, scope
    | StringLiteral str -> ValString str, scope
    | BoolLiteral b -> ValBool b, scope
    | ThisLiteral -> ValReference (thisReference scope), scope
    | ParensExpr e -> evalExpr e scope
    | BinaryExpr(op, e1, e2) -> evalInfix (bbinary op) e1 e2 scope
    | UnaryMinus e -> evalWithFunc e bunaryminus scope
    | UnaryPlus e -> evalWithFunc e bunaryplus scope
    | UnaryNot e -> evalWithFunc e bunarynot scope
    | PropertyAccessor(expr, name) ->
        let v, newScope = evalExpr expr scope
        let ((v, _), newScope) = accessProperty v name newScope
        v, newScope
    | PropertyFunctionCall(expr, name, args) ->
        let v, newScope1 = evalExpr expr scope
        let argVals, newScope2 = evalArgs args newScope1
        functionCallProperty v name argVals newScope2

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
                            (pushScope (scopeLocalName name) (PersistentScope :: List.empty) Map.empty scope)
                    NSSubspace (newVal), popScope sc
            let map, newNewScope = evalGlobalInner remaining newScope
            Map.add name ns map, newNewScope
    evalGlobalInner ns ([globalScopeName],
        Map.add
            [globalScopeName]
            ((PersistentScope :: List.empty), Map.empty)
            Map.empty)


let runMain defns =
    let ons = makeNamespace defns
    let ns, scope = evalGlobals ons
    match Map.tryFind mainFunc ns with
    | Some v ->
        let res, _ = functionCallLocal mainFunc [] scope
        res
    | None -> raise (InterpreterException "No main function found")
       
