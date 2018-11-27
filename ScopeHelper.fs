module ScopeHelper

open InterpreterTypes

let scopeLocalCountName name =
    sprintf "$scopelocalcount_%s$" name

let funcLocalCountName name =
    sprintf "$funclocalcount_%s$" name

let listCountName =
    "$globallistcount"

let globalListName count =
    sprintf "$globallist_%i$" count

let globalScope : NoRefScope = [ScopeGlobal]

let findRef (scope : Scope) : RefType =
    let nrs, refs = scope
    match Map.tryFind nrs refs with
    | None ->
        raise (InterpreterException
            (sprintf "Scope \"%A\" not found in refs" (fst scope)))
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
    pushScope TempBlock List.empty Map.empty scope

let addToMapping name value (scope : Scope) : Scope =
    let nrs, refs = scope
    let opts, mapping = findRef scope
    nrs, Map.add nrs (opts, Map.add name value mapping) refs

let getFromMapping name (scope : Scope) : Value option =
    let _, mapping = findRef scope
    Map.tryFind name mapping

let getLocalCount name scope =
    match getFromMapping name scope with
    | None -> 0
    | Some v ->
        match v with
        | ValInt i -> i
        | _ ->
            raise (InterpreterException
                "Incorrect type for scope local count")

let setLocalCount name count scope =
    addToMapping name (ValInt count) scope

let pushFunctionLocalScope scopeName args (scope : Scope) : Scope =
    let count = getLocalCount (funcLocalCountName scopeName) scope
    let newScope = setLocalCount (funcLocalCountName scopeName) (count + 1) scope
    pushScope (FuncLocal (scopeName, count)) List.empty args newScope

let getListCount scope =
    let _, refs = scope
    match getFromMapping listCountName (globalScope, refs) with
    | None -> 0
    | Some v ->
        match v with
        | ValInt i -> i
        | _ ->
            raise (InterpreterException
                "Incorrect type for list count")

let setListCount count scope =
    let nrs, refs = scope
    let _, newRefs =
        addToMapping listCountName (ValInt count) (globalScope, refs)
    nrs, newRefs

let getListFromRef listRef (scope : Scope) =
    let _, refs = scope
    match getFromMapping listRef (globalScope, refs) with
    | None -> raise (InterpreterException "List not found in refs")
    | Some v ->
        match v with
        | ValListInner innerL -> innerL
        | _ -> raise (InterpreterException "Incorrect type for inner list")

let setListToRef listRef list (scope : Scope) =
    let nrs, refs = scope
    let _, newRefs = addToMapping listRef (ValListInner list) (globalScope, refs)
    nrs, newRefs

let makeNewList argVals newScope =
    let c = getListCount newScope
    let newScope2 = setListCount (c + 1) newScope
    let ref = globalListName c
    ValListReference ref, setListToRef ref argVals newScope2