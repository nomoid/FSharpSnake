module InterpreterTypes

open ProjectParser

type Value =
    | ValNone
    | ValBool of bool
    | ValInt of int
    | ValString of string
    | ValFunc of string list * Stmt list
    | ValList of Value list
    | ValReference of string list
    | ValBuiltinFunc of (Value list -> Scope -> Value * Scope)
and ScopeRules =
    | PersistentScope
and FuncType = string list * Stmt list
and RefType = ScopeRules list * Map<string, Value>
and NoRefScope = string list
and Refs = Map<NoRefScope, RefType>
and Scope = NoRefScope * Refs