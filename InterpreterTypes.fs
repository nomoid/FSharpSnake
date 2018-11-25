module InterpreterTypes

open ProjectParser


type ScopeLayer =
    | ScopeGlobal
    | TempBlock
    | FuncLocal of string
    | ScopeLocal of string
    | ScopeInstance of string * int
type NoRefScope = ScopeLayer list
type ScopeRules =
    | PersistentScope
type FuncType = string list * Stmt list

type Value =
    | ValNone
    | ValBool of bool
    | ValInt of int
    | ValString of string
    | ValFunc of string list * Stmt list
    | ValList of Value list
    | ValReference of ScopeLayer list
    | ValBuiltinFunc of (Value list -> Scope -> Value * Scope)
and RefType = ScopeRules list * Map<string, Value>
and Refs = Map<NoRefScope, RefType>
and Scope = NoRefScope * Refs