module ProjectInterpreter

open ProjectParser
open Builtins

//Consts
let anonArgs = "$anon$"
let tempBlock = "$tempblock$"
let globalScopeName = "$scopeglobal$"

let mainFunc = "main"

type ScopeRules =
| PersistentScope
type FuncType = List<string> * List<Stmt>
type RefType = List<ScopeRules> * Map<string, Value>
type NoRefScope = List<string>
type Refs = Map<NoRefScope, RefType>
type Scope = NoRefScope * Refs

type EvalExpr =
| Unevaled of Expr
| Evaled of Value

type OrderedNamespace =
| ONSSubspace of List<string * OrderedNamespace>
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
    let functionLocalName name =
        sprintf "$funclocal_%s$" name
    pushScope (functionLocalName scopeName) List.empty args scope

let addToMapping (scope : Scope) name value : Scope =
    let nrs, refs = scope
    let opts, mapping = findRef scope
    nrs, Map.add nrs (opts, Map.add name value mapping) refs

//Name resolution & updating

//Note: for this specific function, the first returned scope is the scope of
//the resolved object, not the current scope
let resolveName (scope : Scope) name : Value * Scope =
    let _, refs = scope
    let rec resolveSubspaces (scope : Scope) : Option<Value * Scope> =
        let nrs, refs = scope
        match nrs with
        | [] -> None
        | _ :: remaining ->
            let _, mapping = findRef scope
            match Map.tryFind name mapping with
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
    resV, newScope

let updateName (scope : Scope) name newVal : Scope =
    let noRefScope, _ = scope
    match noRefScope with
    | [] -> raise (InterpreterException "Cannot update empty scope")
    | _ ->
        //Try finding existing value to update
        let rec updateSubspaces (scope : Scope) : Option<Scope> =
            let nrs, refs = scope
            match nrs with
            | [] -> None
            | sname :: remaining ->
                let _, mapping = findRef scope
                match Map.tryFind name mapping with
                | None ->
                    match updateSubspaces (remaining, refs) with
                    | None -> None
                    | Some (innerScope, newRefs) -> 
                        Some (sname :: innerScope, newRefs)
                | Some _ ->
                    Some (addToMapping scope name newVal)
                    
        let currResolveName = updateSubspaces scope      
        match currResolveName with
        | Some v -> v
        | None ->
            //Declare new local variable
            addToMapping scope name newVal

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

//Program execution
let rec executeBlock scope block =
    //Make a new scope
    let newScope = pushTempScope scope
    //printfn "Push: %A" (fst newScope)
    //Eval statements
    let v, newScope2 = evalStmts newScope block
    let popped = popScope newScope2
    //printfn "Pop: %A" (fst newScope2)
    //printfn ""
    //Pop the new scope
    v, popped

and ifOnce scope cond block =
    let v, newScope = evalExpr scope cond
    match v with
    | ValBool b ->
        if b then
            let v, newScope2 = executeBlock newScope block
            Some v, newScope2
        else
            None, newScope
    | _ -> raise (InterpreterException "Condition must be of type bool")
and ifElse scope condBlockList optionBlock =
    match condBlockList with
    | [] ->
        match optionBlock with
        | Some s ->
            executeBlock scope s
        | None ->
            None, scope
    | (cond, block) :: remaining ->
        let ov, newScope = ifOnce scope cond block
        match ov with
        | Some s ->
            s, newScope
        | None ->
            ifElse newScope remaining optionBlock
and runWhile scope cond block =
    let v, newScope = ifOnce scope cond block
    match v with
    // Condition is false
    | None -> None, newScope
    | Some vin -> 
        match vin with
        // Inner return
        | Some _ -> vin, newScope
        // Tail call while loop
        | None -> runWhile newScope cond block
and evalStmt (nsScope : Scope) stmt =
    match stmt with
    | FunctionCallStmt(name, args) ->
        let argVals, newScope = evalArgs nsScope args
        let _, newScope2 = functionCall newScope name argVals
        None, newScope2
    | AssignmentStmt(name, expr) ->
        let v, newScope = evalExpr nsScope expr
        None, updateName newScope name v
    | LetStmt(name, expr) ->
        let v, newScope = evalExpr nsScope expr
        None, addToMapping newScope name v
    | ReturnStmt expr -> 
        let v, newScope = evalExpr nsScope expr
        Some v, newScope
    | IfElseStmt((cond, block), condBlockList, optionBlock) ->
        ifElse nsScope ((cond, block) :: condBlockList) optionBlock
    | WhileStmt(cond, block) ->
        runWhile nsScope cond block
and evalStmts (nsScope : Scope) stmts =
    //printfn "%A" (fst nsScope)
    match stmts with
    | [] -> None, nsScope
        //raise (InterpreterException "Function has no return value")
    | stmt :: remaining ->
        let evalStmtResult, newScope = evalStmt nsScope stmt
        match evalStmtResult with
        | Some v -> Some v, newScope
        | None -> evalStmts newScope remaining
and functionCall (scope : Scope) name args =
    //printfn "Push f: %A" (fst scope)
    let nrsOuter, _ = scope
    let (func, scopeInner) = resolveName scope name
    match func with
    | ValFunc (argNames, stmts) ->
        let argsMap = makeArgsMap argNames args
        //Push function local scope
        let newScope = pushFunctionLocalScope name argsMap scopeInner
        let ov, newScope2 = evalStmts newScope stmts
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
and evalArgs (nsScope : Scope) args =
    match args with
    | [] -> [], nsScope
    | arg :: remaining ->
        let v, newScope = evalExpr nsScope arg
        let vals, finalScope = evalArgs newScope remaining
        v :: vals, finalScope
and evalExpr (nsScope : Scope) expr : (Value * Scope) =
    match expr with
    | FunctionCallExpr (name, args) ->
        let argVals, newScope = evalArgs nsScope args
        functionCall newScope name argVals
    | Identifier name ->
        let (v, _) = resolveName nsScope name
        v, nsScope
    | NumLiteral n -> ValInt n, nsScope
    | StringLiteral str -> ValString str, nsScope
    | BoolLiteral b -> ValBool b, nsScope
    | ParensExpr e -> evalExpr nsScope e
    | BinaryExpr(op, e1, e2) -> evalInfix nsScope (bbinary op) e1 e2
and evalInfix (scope : Scope) processor e1 e2 =
    let vLeft, newScope1 = evalExpr scope e1
    let vRight, newScope2 = evalExpr newScope1 e2
    processor vLeft vRight, newScope2
  
let rec convertToNamespace (ns : List<string * OrderedNamespace>) =
    match ns with
    | [] -> Map.empty
    | pair :: remaining ->
        let map = convertToNamespace remaining
        let name, ons = pair
        let ns = 
            match ons with
            | ONSVar v -> NSVar v
            | ONSFunc(args, body) -> NSFunc(args, body)
            | ONSSubspace list -> NSSubspace (convertToNamespace list)
        Map.add name ns map

let rec evalGlobals (ns : List<string * OrderedNamespace>) =
    let scopeLocalName name =
        sprintf "$scopelocal_%s$" name
    let firstPass = convertToNamespace ns
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
                        let newVal, sc = evalExpr scope expr
                        NSVar (Evaled newVal), addToMapping sc name newVal
                    | Evaled value -> 
                        raise (InterpreterException 
                            "Expr shound not be already evaluated")
                | ONSFunc(args, body) -> 
                    NSFunc(args, body), 
                        addToMapping scope name (ValFunc(args, body))
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
        let res, _ = functionCall scope mainFunc []
        res
    | None -> raise (InterpreterException "No main function found")
        
