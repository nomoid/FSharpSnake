open System.IO

open ProjectParser
open ProjectInterpreter

exception IllegalArgumentException of string

[<EntryPoint>]
let main argv =
    try
        let s = argv.[0]
        let lines = File.ReadLines s |> List.ofSeq

        match parseComplete lines with
        | None -> raise (IllegalArgumentException s)
        | Some dlist -> 
            let s2 = (prettyprint dlist)
            //printfn "%s" s2
            //printfn "%O" dlist
            match runMain dlist with
            | ValInt i ->
                printfn "Program exited with return code %i" i
            | _ ->
                printfn "Program exited with non-integer return code"
            0 // return an integer exit code*)
    with
        | IllegalArgumentException s ->
            printfn "Cannot parse input from \"%s\"" s
            printfn "Usage: dotnet run <s>"
            1
        | InterpreterException s ->
            printfn "Interpreter exception \"%s\"" s
            printfn "Usage: dotnet run <s>"
            1
        //| _ -> printfn "Usage: dotnet run <s>"; 1
