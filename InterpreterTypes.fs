module InterpreterTypes

open ProjectParser

exception InterpreterException of string

type ScopeLayer =
    | ScopeGlobal
    | TempBlock
    | FuncLocal of string * int
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
    | ValListReference of string
    | ValReference of NoRefScope
    | ValBuiltinFunc of (Value list -> Scope -> Value * Scope)
    // Fake value types
    | ValListInner of Value list
    | ValOrderedNamespace of (string * OrderedNamespace) list
and RefType = ScopeRules list * Map<string, Value>
and Refs = Map<NoRefScope, RefType>
and Scope = NoRefScope * Refs
and EvalExpr =
    | Unevaled of Expr
    | Evaled of Value
and OrderedNamespace =
    | ONSSubspace of (string * OrderedNamespace) list
    | ONSVar of EvalExpr
    | ONSFunc of FuncType

type Namespace =
    | NSSubspace of Map<string, Namespace>
    | NSVar of EvalExpr
    | NSFunc of FuncType