module ProjectParser

open System
open System.Text.RegularExpressions
open Parser

type BinaryOp =
    // Numerical operations
    | Add
    | Sub
    | Mult
    | Div
    | Mod
    // Boolean operations
    | And
    | Or
    // Comparison operations
    | Eq
    | Neq
    | Leq
    | Geq
    | Lt
    | Gt
    | Dot

let binaryOps = [
    Add; Sub; Mult; Div; Mod; And; Or; Eq; Neq; Leq; Geq; Lt; Gt; Dot
]
let assignmentOps = [
    Add; Sub; Mult; Div; Mod; And; Or
]
let precedences = [
    [Dot];
    [Mult; Div; Mod]; 
    [Add; Sub]; 
    [Leq; Geq; Lt; Gt]; 
    [Eq; Neq]; 
    [And]; 
    [Or]
]

let optostr op =
    match op with
    | Add -> "+"
    | Sub -> "-"
    | Mult -> "*"
    | Div -> "/"
    | Mod -> "%"
    | Eq -> "=="
    | Neq -> "!="
    | Leq -> "<="
    | Geq -> ">="
    | Lt -> "<"
    | Gt -> ">"
    | And -> "&"
    | Or -> "|"
    | Dot -> "."

type Expr =
    | FunctionCallExpr of string * Expr list
    | NumLiteral of int
    | StringLiteral of string
    | BoolLiteral of bool
    | ListLiteral of Expr list
    | ThisLiteral
    | ParensExpr of Expr
    | BinaryExpr of BinaryOp * Expr * Expr
    | UnaryMinus of Expr
    | UnaryPlus of Expr
    | UnaryNot of Expr
    | PropertyFunctionCall of Expr * string * Expr list
    | LValueExpr of LValue
// Value that can appear at the left side of an assignment
and LValue =
    | PropertyAccessor of Expr * string
    | PropertyArrayAccessor of Expr * string * Expr list
    | ArrayAccessor of Expr * Expr list
    | Identifier of string

type Stmt =
    | FunctionCallStmt of string * Expr list
    | AssignmentStmt of LValue * BinaryOp option * Expr
    | LetStmt of string * Expr
    | ReturnStmt of Expr
    | IfElseStmt of
        (Expr * Stmt list) *
        ((Expr * Stmt list) list) *
        Stmt list option
    | WhileStmt of Expr * Stmt list

type Defn =
    // Function with (name, arg_names[], body)
    | FunctionDefn of string * string list * Stmt list
    | ScopeDefn of string * Defn list
    | AssignmentDefn of string * Expr

let prettyprintcommasep stringify exprs =
    (List.fold (fun a b ->
            (if a <> "" then a + ", " else "") + stringify b
        ) "" exprs)

let prettyprintfunc stringify exprs =
    "(" + (prettyprintcommasep stringify exprs) + ")"


let rec prettyprintexpr expr =
    match expr with
    | FunctionCallExpr(name, exprs) -> prettyprintcall name exprs
    | NumLiteral(num) -> sprintf "%i" num
    | StringLiteral(str) -> sprintf "\"%s\"" str
    | BoolLiteral(b) -> if b then "true" else "false"
    | ListLiteral(exprs) ->
        "[" + (prettyprintcommasep prettyprintexpr exprs) + "]"
    | ThisLiteral -> "this"
    | ParensExpr(e) -> sprintf "(%s)" (prettyprintexpr e)
    | BinaryExpr(op, e1, e2) -> prettyprintinfix (optostr op) e1 e2
    | UnaryMinus(e) -> sprintf "-%s" (prettyprintexpr e)
    | UnaryPlus(e) -> sprintf "+%s" (prettyprintexpr e)
    | UnaryNot(e) -> sprintf "!%s" (prettyprintexpr e)
    | PropertyFunctionCall(expr, name, exprs) ->
        sprintf "%s.%s"
            (prettyprintexpr expr)
            (prettyprintcall name exprs)
    | LValueExpr lv ->
        prettyprintlvalue lv
and prettyprintlvalue lv =
    match lv with
    | Identifier name -> name
    | PropertyAccessor(expr, name) -> 
        sprintf "%s.%s"
            (prettyprintexpr expr)
            name
    | PropertyArrayAccessor(expr, name, exprs) ->
        sprintf "%s.%s%s"
            (prettyprintexpr expr)
            name
            (prettyprintarrayaccess exprs)
    | ArrayAccessor(expr, exprs) ->
        sprintf "%s%s"
            (prettyprintexpr expr)
            (prettyprintarrayaccess exprs)
and prettyprintarrayaccess exprs =
    let wrapWithArray str =
        sprintf "[%s]" str
    exprs 
    |> List.map prettyprintexpr 
    |> List.map wrapWithArray 
    |> String.concat ""
and prettyprintcall name exprs =
    name + ((prettyprintfunc prettyprintexpr) exprs)
and prettyprintinfix op e1 e2 =
    sprintf "%s %s %s"
            (prettyprintexpr e1) op (prettyprintexpr e2)

let prettyprintassignment lv oop expr =
    let opPart =
        match oop with
        | None -> ""
        | Some op -> optostr op
    sprintf "%s %s= %s" (prettyprintlvalue lv) opPart (prettyprintexpr expr)

let prettyprintlet name expr =
    "let " + (prettyprintassignment (Identifier name) None expr)

// Currently auto-detects indentation size
let defaultIndentationSize = 4
let indentation = String.replicate defaultIndentationSize " "

let indent xs =
    List.map (fun s -> indentation + s) xs

let rec prettyprintgroup header innerPrinter block =
    header + ":"
            :: (indent (block |> List.collect innerPrinter))

let rec prettyprintstmt stmt =
    match stmt with
    | FunctionCallStmt(name, exprs) -> [prettyprintcall name exprs]
    | AssignmentStmt(lv, op, expr) -> 
        [prettyprintassignment lv op expr]
    | LetStmt(name, expr) -> [prettyprintlet name expr]
    | ReturnStmt(expr) -> ["return " + prettyprintexpr expr]
    | IfElseStmt((cond, block), condBlockList, optionBlock) ->
        let elseLines =
            match optionBlock with
            | None -> []
            | Some v -> prettyprintgroup "else" prettyprintstmt v
        List.concat
            [
                prettyprintgroup ("if " + prettyprintexpr cond)
                    prettyprintstmt block
                List.collect (fun (innerCond, innerBlock) ->
                    (prettyprintgroup ("elif " + prettyprintexpr innerCond)
                        prettyprintstmt innerBlock)
                    ) condBlockList
                elseLines
            ]
    | WhileStmt(cond, block) ->
        prettyprintgroup ("while " + prettyprintexpr cond)
                    prettyprintstmt block


let prettyprintlist printer xs =
    xs
    |> List.map (printer >> (List.fold (fun a b -> a + "\n" + b) ""))
    |> List.fold (+) ""

let rec prettyprintdef def =
    match def with
    | FunctionDefn(name, args, body) ->
        prettyprintgroup (name + (prettyprintfunc id args))
            prettyprintstmt body
    | ScopeDefn(name, body) ->
        prettyprintgroup name prettyprintdef body
    | AssignmentDefn(name, expr) -> 
        [prettyprintassignment (Identifier name) None expr]

let prettyprint =
    prettyprintlist prettyprintdef

let poption (parser: Parser<'a>) input : Outcome<'a Option> =
    match parser input with
    | Success (res, rem) -> Success (Some res, rem)
    | Failure -> Success (None, input)

let pmapoption (parser: Parser<'a option>) input : Outcome<'a> =
    match parser input with
    | Failure -> Failure
    | Success (resv, rem) ->
        match resv with
        | None -> Failure
        | Some s -> Success(s, rem)   

let isSymb c = is_regexp (c.ToString()) @"[_$]"
let isLetterSymb c = is_letter c || isSymb c
let isValidChar c = isLetterSymb c || is_digit c
let isNotQuote c = c <> '\"'

let psymb = psat isSymb
let plettersymb = psat isLetterSymb
let pvalidchar = psat isValidChar
let charListToString str =
    str |> List.map (fun x -> x.ToString()) |> String.concat ""
let pidentifier =
    pseq plettersymb (pmany0 pvalidchar)
        (fun (x,xs) ->
            (x :: xs)
            |> charListToString
        )

let pidsep = (poption (pchar '?'))
let pid = pidentifier |>> Identifier

let plistsep sepParser innerParser =
    poption (pseq (pmany0 (pleft innerParser sepParser)) innerParser
        (fun (xs, x) -> List.append xs [x])) |>>
        (fun x ->
            match x with
            | Some y -> y
            | None -> List.empty
        )

let pFuncHelper innerParser =
    pseq (pleft pidentifier (pchar '('))
        (pleft
            (plistsep (pchar ',') innerParser)
            (pchar ')')
        )
        (fun (name, args) -> (name, args))


let (pExpr : Parser<Expr>), pExprImpl = recparser()

let pFuncCall = pFuncHelper pExpr

let pFuncCallExpr = pFuncCall |>> FunctionCallExpr
let pFuncCallStmt = pFuncCall |>> FunctionCallStmt

let tryParseInt (s : string) =
    match System.Int32.TryParse(s) with
    | (true, v) -> Some v
    | (false, _) -> None

let pPosInt = pmapoption (pmany1 pdigit |>> charListToString |>> tryParseInt)

let pNumLiteral =
    pseq (poption (pchar '-'))
        pPosInt
        (fun (oc, num) ->
            match oc with
            | Some _ -> -num
            | None -> num
        ) |>> NumLiteral

//String parsing is handled by the upper parser

//let pStrInner = (pright (pchar '\\') pitem) <|> (psat isNotQuote)
//let pStrLiteral =
//    pleft (pright (pchar '"') (pmany0 pStrInner)) (pchar '"')
//        |>> charListToString
//        |>> StringLiteral

let pThisLiteral = pfresult (pstr "this") (ThisLiteral)
let pTrueLiteral = pfresult (pstr "true") (BoolLiteral true)
let pFalseLiteral = pfresult (pstr "false") (BoolLiteral false)
let pBoolLiteral = pTrueLiteral <|> pFalseLiteral
let pListLiteral = 
    pbetween 
        (pchar '[') 
        (pchar ']') 
        (plistsep (pchar ',') pExpr)
        |>> ListLiteral

let pParens = pbetween (pchar '(') (pchar ')') pExpr |>> ParensExpr

let pUnaryMinus = pright (pchar '-') pExpr |>> UnaryMinus

let pUnaryPlus = pright (pchar '+') pExpr |>> UnaryPlus

let pUnaryNot = pright (pchar '!') pExpr |>> UnaryNot

let pUnaryOp = pUnaryMinus <|> pUnaryPlus <|> pUnaryNot

let pConsumingExpr =
    pUnaryOp
    <|> pFuncCallExpr
    <|> pThisLiteral
    <|> pBoolLiteral
    <|> (pid |>> LValueExpr)
    <|> pNumLiteral
    <|> pParens
    <|> pListLiteral
//    <|> pStrLiteral

let makebin bop (a, b) =
    if bop = Dot then
        match b with
        | LValueExpr lv ->
            match lv with
            | Identifier i -> Some ((PropertyAccessor(a, i)) |> LValueExpr)
            | ArrayAccessor (expr, args) ->
                match expr with
                | LValueExpr lv ->
                    match lv with
                    | Identifier i -> 
                        Some 
                            ((PropertyArrayAccessor(a, i, args)) 
                            |> LValueExpr)
                    | _ -> None
                | _ -> None
            | _ -> None
        | FunctionCallExpr (name, args) -> 
            Some (PropertyFunctionCall(a, name, args))
        | _ -> None
    else
        Some (BinaryExpr (bop, a, b))

let pbinop op =
    let str = (optostr op)
    let exprgen = makebin op
    pfresult (pstr str) (op, exprgen)

let pinfixop opList =
    List.fold (fun parser op -> (parser <|> (pbinop op))) pzero opList

let rec combineSinglePrecOp right cexpr xs predicate =
    match xs with
    | [] -> Some (cexpr, [])
    | ((name, op), term) :: remaining ->
        match combineSinglePrecOp right term remaining predicate with
        | None -> None
        | Some (expr, newRemaining) ->
            if predicate name then
                let opin = if right then (cexpr, expr) else (expr, cexpr)
                match (op opin) with
                | None -> None
                | Some v -> Some (v, newRemaining)
            else
                Some (cexpr, ((name, op), expr) :: newRemaining)

let rec rightToLeftAssoc cexpr xs ys =
    match xs with
    | [] -> cexpr, ys
    | (opn, expr) :: remaining ->
        rightToLeftAssoc expr remaining ((opn, cexpr) :: ys)

let rec combineOps right precs (cexpr, xs) =
    match precs with
    | [] ->
        match combineSinglePrecOp right cexpr xs (fun _ -> true) with
        | None -> None
        | Some (newExpr, _) -> Some newExpr
    | singlePrec :: remaining ->
        let res = 
            combineSinglePrecOp right cexpr xs
                (fun x -> List.contains x singlePrec)
        match res with
        | None -> None
        | Some (newExpr, nxs) -> combineOps right remaining (newExpr, nxs)

let combineOpsRight precs (cexpr, xs) =
    combineOps true precs (cexpr, xs)

let combineOpsLeft precs (cexpr, xs) =
    combineOps false precs (rightToLeftAssoc cexpr xs [])

let pArrayAccessSingle =
    pbetween (pchar '[') (pchar ']') pExpr

let pArrayAccessExpr =
    pseq pConsumingExpr (pmany1 pArrayAccessSingle) ArrayAccessor

let pInnerExpr =
    (pArrayAccessExpr |>> LValueExpr) <|> pConsumingExpr

pExprImpl :=
    pmapoption
        (pseq pInnerExpr
            (pmany0
                (pseq (pinfixop binaryOps)
                    pInnerExpr
                    id
                )
            )
            (combineOpsLeft precedences))
   
let pAssignmentExpr input =
    match pExpr input with
    | Failure -> Failure
    | Success (expr, rem) ->
        match expr with
        | LValueExpr lv ->
            Success (lv, rem)
        | _ -> Failure

let pOpEq =
    pleft (poption (pinfixop assignmentOps)) (pchar '=') |>> Option.map fst

let pComplexAssignment =
    pseq (pseq pAssignmentExpr pOpEq id) pExpr id
let pSimpleAssignment =
    pseq (pleft pidentifier (pchar '=')) pExpr id
let pAssignmentStmt = 
    pComplexAssignment 
    |>> (fun ((a, b), c) -> a, b, c)
    |>> AssignmentStmt
let pAssignmentGlobal = pSimpleAssignment |>> AssignmentDefn


let pIdAssign =
    pseq (pleft pidentifier pidsep) pSimpleAssignment id

let pIdExpr =
    pseq (pleft pidentifier pidsep) pExpr id

let pWordStmt pidx str outputProcessor input =
    let outcome = pidx input
    match outcome with
    | Success ((iden, parsed), remaining) ->
        if iden = str then
            Success (outputProcessor parsed, remaining)
        else
            Failure
    | Failure -> Failure

let pWordAssignStmt = pWordStmt pIdAssign "let" LetStmt

let pWordExprStmt = pWordStmt pIdExpr "return" ReturnStmt

let pStmt =
    pAssignmentStmt
    <|> pWordExprStmt
    <|> pWordAssignStmt
    <|> pFuncCallStmt

// Use mutable to keep track of max line reached
// For program debugging purposes
let mutable maxLine = 0

let pPosIntMut input =
    let retVal = pPosInt input
    match retVal with
    | Success (v, _) ->
        maxLine <- if v > maxLine then v else maxLine
        retVal
    | Failure -> retVal

let pMaxLine parser = pright pPosIntMut (pright (pstr "{") parser)

let pLine parser = pbetween (pstr "{l") (pstr "}l}") (pMaxLine parser)
let pBlock parser = pbetween (pstr "{b{") (pstr "}b}") (pmany1 parser)
let pFuncHeader = pFuncHelper pidentifier

let pGroup header blockElem converter =
    pbetween (pstr "{s") (pstr "}s}") (pMaxLine
        (
            pseq header (pBlock blockElem)
                converter
        ))

let pIfHeader = pWordStmt pIdExpr "if" id
let pElifHeader = pWordStmt pIdExpr "elif" id
let pElseHeader = pWordStmt (pidentifier |>> (fun a -> a, ())) "else" id

//IfElseStmt((name, block), List.empty, None))

let pWhileHeader = pWordStmt pIdExpr "while" id

let rec pFuncScope header =
    let b : Parser<Stmt> = ((pLine pStmt) <|> pInnerScope)
    pGroup header b
and pIfGroup a =
    a |>
        (pseq
            (pseq (pFuncScope pIfHeader id)
                (pmany0 (pFuncScope pElifHeader id))
                id
            ) (poption (pFuncScope pElseHeader (fun (_, stmts) -> stmts)))
            (fun (((cond, block), condBlockList), optionBlock) ->
                IfElseStmt((cond, block), condBlockList, optionBlock))
        )
and pWhileGroup a =
    a |> pFuncScope pWhileHeader WhileStmt
and pInnerScope a = a |> (pIfGroup <|> pWhileGroup)

let (pDefn : Parser<Defn>), pDefnImpl = recparser()

let pScope =
    pGroup pidentifier pDefn
        (fun (name, defns) -> ScopeDefn(name, defns))

let pFunc =
    pFuncScope pFuncHeader
        (fun ((name, args), block) -> FunctionDefn(name, args, block))

pDefnImpl := pFunc <|> pScope <|> (pLine pAssignmentGlobal)
let pProgram = pmany1 pDefn

let grammar = pleft pProgram peof

let parseLower input : Defn list option =
    maxLine <- 0
    match grammar (prepare input) with
    | Success(e,_) -> Some e
    | Failure -> None

let cleanLower input =
    let clean0 = Regex.Replace(input, @"[\n\r]+", "")
    let clean1 = Regex.Replace(clean0, @"[\s]{2,}", " ")
    let clean2 =
        Regex.Replace(clean1, @"([A-Za-z0-9_]) ([A-Za-z0-9_])", "$1?$2")
    let clean3 = Regex.Replace(clean2, @" ", "")
    clean3


type Block =
    | NoBlock
    | Line of int * string
    | SubBlock of int * string * Block list

let linewithnum num l =
    sprintf "%04i:%s" num l

let rec prettyprintblock block =
    match block with
    | NoBlock -> []
    | Line (_, l) -> [l]
    | SubBlock(_, title, sub) ->
        title + ":" :: (indent (sub |> List.collect prettyprintblock))

let initialSpacing str =
    let rec initialSpacingInner clist =
        match clist with
        | [] -> 0
        | ' ' :: xs -> 1 + initialSpacingInner xs
        | _ -> 0
    initialSpacingInner (Seq.toList str)

let spacesOf count =
    String.replicate count " "

let rec findIndented (lineList : (int * string) list) indentationAmount =
    match lineList with
    | [] -> [], []
    | (num, line) :: remaining ->
        let actualAmount =
            if indentationAmount = 0 then
                initialSpacing line
            else
                indentationAmount
        if actualAmount > 0 &&
                line.Length >= actualAmount && 
                line.Substring(0, actualAmount) = spacesOf actualAmount then
            let unindented = line.Substring(actualAmount)
            let remainingIndented, rest = findIndented remaining actualAmount
            (num, unindented) :: remainingIndented, rest
        else
            [], lineList
let rec parseUpperNum lineList =
    match lineList with
    | [] -> []
    | (num, line) :: remaining ->
        let cleanLine = Regex.Replace(line, @":\s+$", ":")
        if cleanLine.Chars(cleanLine.Length - 1) = ':' then
            let cleanLine2 = cleanLine.Substring(0, cleanLine.Length - 1)
            let indented, rest = findIndented remaining 0
            (SubBlock(num, cleanLine2, parseUpperNum indented)) 
                :: (parseUpperNum rest)
        else
            Line (num, line) :: (parseUpperNum remaining)

let parseUpper (stringList : string list) =
    let zipped = List.zip [1..stringList.Length] stringList
    let newLines = List.filter (fun x -> snd x <> "") zipped
    parseUpperNum newLines

let stringExtract i =
    sprintf "$str_%i$" i

let stringExtractRegex i =
    sprintf "$$str_%i$$" i

let stringReplace (str : string) index length replacement =
    String.concat "" [str.[0 .. index - 1];
                        replacement;
                        str.[index + length .. str.Length - 1]]

let extractQuotes line (strings : string[]) =
    let quoteRegex = new Regex("(\"([^\"\\\\]|\\\\\\\\|\\\\\")+\")")
    let m = quoteRegex.Match(line)
    if m.Success then
        let firstMatch = List.head (Seq.toList m.Groups)
        let text = firstMatch.Value
        let text2 = Regex.Replace(text, "\\\\\\\\", "\\\\")
        let text3 = Regex.Replace(text2, "\\\\\"", "\"")
        let outLine =
            quoteRegex.Replace(line, stringExtract strings.Length, 1)
        //Text contains quotes, so remove first and last char
        outLine, Array.append strings [|text3.[1 .. text3.Length - 2]|]
    else
        line, strings

let cleanUpperLine line strings =
    //Extract string literals - do this before other cleanup
    let newLine, newStrings = extractQuotes line strings
    //Replace illegal chars
    let clean0 = Regex.Replace(newLine, @"[\{\}]", "")
    //Replace comments
    let clean1 = Regex.Replace(clean0, @"\#.*", "")
    if Regex.Replace(clean1, @"\s", "") = "" then
        "", newStrings
    else
        clean1, newStrings

let cleanUpper stringList =
    let cleanUpperInner str (xs, assigns) =
        let cleaned, newAssigns = cleanUpperLine str assigns
        cleaned :: xs, newAssigns
    let newLines, strings = List.foldBack cleanUpperInner stringList ([], [||])
    newLines, strings

type UpperToLowerResult =
    | UTLSuccess of string
    | UTLFailure of int

let rec upperToLower blockList =
    let upperToLowerInner state block =
        match state with
        | UTLFailure num -> UTLFailure num
        | UTLSuccess s ->
            match upperToLowerSingle block with
            | UTLFailure num -> UTLFailure num
            | UTLSuccess s2 -> UTLSuccess (s + s2)
    List.fold upperToLowerInner (UTLSuccess "") blockList
and upperToLowerSingle block =
    match block with
    | NoBlock -> UTLSuccess ""
    | Line (num, line) ->
        //Make sure line does not start with spaces
        if line.StartsWith " " then
            UTLFailure num
        else
            UTLSuccess ((sprintf "{l%i{" num) + line + "}l}")
    | SubBlock(num, title, sub) ->
        if title.StartsWith " " then
            UTLFailure num
        else
            match upperToLower sub with
            | UTLFailure num -> UTLFailure num
            | UTLSuccess v -> UTLSuccess ((sprintf "{s%i{" num) + title + "{b{" + v + "}b}}s}")

let assignStrings assigns =
    let xs = Array.toList assigns
    let xsz = List.zip [0..(xs.Length-1)] xs
    List.map (fun (index, elem) ->
        AssignmentDefn(stringExtract index, StringLiteral(elem))) xsz

type ParseResult =
    | ParseSuccess of Defn list
    //Line failed on
    | ParseFailure of int option

let parseComplete lines =
    let cleanLines, assigns = cleanUpper lines
    let parsed = parseUpper cleanLines
    match upperToLower parsed with
    | UTLFailure num -> ParseFailure (Some num)
    | UTLSuccess lower ->
        let cleaned = cleanLower lower
        match parseLower cleaned with
        | None ->
            let res = (Some maxLine)
            maxLine <- 0
            ParseFailure res
        | Some s ->
            maxLine <- 0
            ParseSuccess (List.append (assignStrings assigns) s)
        