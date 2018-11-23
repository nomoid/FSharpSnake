(*
  A simple combinator-style parsing library for F#.

  Inspired by the Hutton & Meijer paper as well as the FParsec
  combinator library.  Other than being much smaller, this
  library trades away performance for simplicity.  If you need
  a fast library, look at FParsec.

  Version: 1.1 (2018-10-28)
*)

module Parser

open System
open System.Text.RegularExpressions

type Input = string * bool

type Outcome<'a> =
    | Success of result: 'a * remaining: Input
    | Failure

type Parser<'a> = Input -> Outcome<'a>

let recparser() =
  let dumbparser = fun (input: Input) -> failwith "You forgot to initialize your recursive parser."
  let r = ref dumbparser
  (fun (input: Input) -> !r input), r

let is_regexp(s: string)(rgx: string) =
    Regex.Match(s, rgx).Success

let is_whitespace(c: char) = is_regexp (c.ToString()) @"\s"

let is_upper(c: char) = is_regexp (c.ToString()) @"[A-Z]"

let is_lower(c: char) = is_regexp (c.ToString()) @"[a-z]"

let is_letter(c: char) = is_upper c || is_lower c

let is_digit(c: char) = is_regexp (c.ToString()) @"[0-9]"

let presult(a: 'a)(i: Input) : Outcome<'a> = Success(a,i)

let pzero(i: Input) : Outcome<'a> = Failure

let pitem(i: Input) : Outcome<char> =
    let istr = fst i
    if istr = "" then
        Failure
    else
        let at_end = snd i
        Success (istr.[0], (istr.[1..], at_end))

let pbind(p: Parser<'a>)(f: 'a -> Parser<'b>)(i: Input) : Outcome<'b> =
    match p i with
    | Success(a,i') -> f a i'
    | Failure -> Failure

let pseq(p1: Parser<'a>)(p2: Parser<'b>)(f: 'a*'b -> 'c) : Parser<'c> =
    pbind p1 (fun a ->
        pbind p2 (fun b ->
            presult (f (a,b))
        )
    )

let psat(f: char -> bool) : Parser<char> =
    pbind pitem (fun c -> if (f c) then presult c else pzero)

let pchar(c: char) : Parser<char> = psat (fun c' -> c' = c)

let pletter : Parser<char> = psat is_letter

let pdigit : Parser<char> = psat is_digit

let pupper : Parser<char> = psat is_upper

let plower : Parser<char> = psat is_lower

let (<|>)(p1: Parser<'a>)(p2: Parser<'a>)(i: Input) : Outcome<'a> =
    let o = p1 i
    match o with
    | Success(_,_) -> o
    | Failure -> p2 i

let pfun(p: Parser<'a>)(f: 'a -> 'b)(i: Input) : Outcome<'b> =
    let o = p i
    match o with
    | Success(a,i') -> Success(f a, i')
    | Failure -> Failure

let (|>>)(p: Parser<'a>)(f: 'a -> 'b) : Parser<'b> = pfun p f

let pfresult(p: Parser<'a>)(x: 'b) : Parser<'b> =
    pbind p (fun a -> presult x)

let rec pmany0(p: Parser<'a>)(i: Input) : Outcome<'a list> =
    let rec pm0(xs: 'a list)(i: Input) : Outcome<'a list> =
        match p i with
        | Failure        -> Success(xs, i)
        | Success(a, i') ->
            if i = i' then
                failwith "pmany parser loops infinitely!"
            pm0 (a::xs) i'
    match pm0 [] i with
    | Success(xs,i') -> Success(List.rev xs, i')
    | Failure        -> Failure

let pmany1(p: Parser<'a>) : Parser<'a list> =
    pseq p (pmany0 p) (fun (x,xs) -> x :: xs)

let pws0 : Parser<char list> = pmany0 (psat is_whitespace)

let pws1 : Parser<char list> = pmany1 (psat is_whitespace)

let pstr(s: string) : Parser<string> =
    s.ToCharArray()
    |> Array.fold (fun pacc c ->
                      pseq pacc (pchar c) (fun (s,ch) -> s + ch.ToString())
                  ) (presult "")

let pnl : Parser<string> =
    (pfun (psat (fun c -> c = '\n')) (fun c -> c.ToString()))
    <|> (pstr @"\r\n")

let peof(i: Input) : Outcome<bool> =
    match pitem i with
    | Failure ->
        if snd i = true then
            Success(true, i)
        else
            Failure
    | Success(_,_) -> Failure

let pleft(pleft: Parser<'a>)(pright: Parser<'b>) : Parser<'a> =
    pbind pleft (fun a -> pfresult pright a)

let pright(pleft: Parser<'a>)(pright: Parser<'b>) : Parser<'b> =
    pbind pleft (fun _ -> pright)

let pbetween(popen: Parser<'a>)(pclose: Parser<'b>)(p: Parser<'c>) : Parser<'c> =
    pright popen (pleft p pclose)

let (<!>)(p: Parser<'a>)(label: string)(i: Input) : Outcome<'a> =
    let o = p i
    match o with
    | Success(a, i') ->
        let istr  = fst i
        let istr' = fst i'
        let nconsumed = istr.Length - istr'.Length
        let iconsumed = istr.[0..(nconsumed - 1)]
        printfn "[success: %s, consumed: \"%s\", remaining: \"%s\"]" label iconsumed istr'
    | Failure        ->
        let istr = fst i
        printfn "[failure: %s, remaining input: \"%s\"]" label istr
    o

let stringify(cs: char list) : string = String.Join("", cs)

let prepare(input: string) : Input = input, true
