open System.IO

open ProjectParser
open ProjectInterpreter

exception ParseFailureException of Option<int * string>

//TODO

[<EntryPoint>]
let main argv =
    try
        let s = argv.[0]
        let lines = File.ReadLines s |> List.ofSeq

        match parseComplete lines with
        | ParseFailure onum ->
            let oline = Option.map (fun x -> x, lines.[x-1]) onum
            raise (ParseFailureException oline)
        | ParseSuccess dlist ->
            //let s2 = (prettyprint dlist)
            //printfn "%s" s2
            //printfn "%O" dlist
            match runMain dlist with
            | ValInt i ->
                printfn "Program exited with return code %i" i
            | _ ->
                printfn "Program exited with non-integer return code"
            0 // return an integer exit code*)
    with
        | ParseFailureException oline ->
            let oval =
                match oline with
                | None -> "Cannot parse input"
                | Some (num, line) -> sprintf "Invalid syntax on line %i:\n\t%s" num (line.Trim())
            printfn "%s" oval
            1
        | InterpreterException s ->
            printfn "Interpreter exception \"%s\"" s
            1
        //| _ -> printfn "Usage: dotnet run <s>"; 1
