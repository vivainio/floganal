open System.IO
open System.Text.RegularExpressions
open System.Diagnostics


let (|ParseRegex|_|) regex str =
   let m = Regex(regex).Match(str)
   if m.Success
   then Some (List.tail [ for x in m.Groups -> x.Value ])
   else None

let readLines fname = File.ReadAllLines fname

type Error = {
    Code: string;
    Text: string;
    File: string
}

type Event =
    | Warn of Error
    | Error of Error
    | Boundary of string


let parseLine line =
    match line with 
        | ParseRegex ".*warning (CS.*?):(.*)\[(.*)\]" [code; text; file] 
         -> Some (Warn { Code = code; Text = text; File = file })
        | ParseRegex "Building project: (.*)"  [prjname] -> Some(Boundary prjname)
        | _ -> None

let render event = 
    match event with 
        | Warn d -> sprintf "%s\t%s" d.Code d.Text
        | Boundary f -> sprintf " ** %s" f
        | _ -> ""

type State = string * (string * Event) list
let assignBoundary (state: State) (event: Event) =
    let current, all = state
    match event with
        | Boundary f -> (f, all)
        | Warn d -> (current, all @ [(current, event)])
        | _ -> state
    
[<DebuggerDisplay("{Module}: {Events}")>]
type Module = {
    Module: string;
    Events: Event[]
}

let groupByToArray f sq = Seq.groupBy f sq  |> Seq.map (fun (k,els) -> k, Array.ofSeq <| Seq.map snd els)


let warningStats (events: Event seq) = 

    let grouped = events |> Seq.choose (fun ev -> match ev with 
        | Warn d -> Some (d.Code, d)
        | _ -> None) |> groupByToArray fst 
        
    grouped |> Seq.map (fun (code, errs) -> code, errs.Length, errs.[0].Text)

let errorCategory (err:string) =
    let cat = Config.errorCodeCats |>
                Seq.tryFind (fun (codes, result) -> List.exists ((=) err) codes) 
    match cat with 
    | Some c -> snd c
    | None -> err


let printModuleStats (m: Module) = 
    printfn "%s" m.Module
    for (code, count, text) in warningStats m.Events do 
        printfn "\t%d\t%s\t%s " count (errorCategory code) text 

let analyzeLog fname = 
    let content = readLines fname
    let events = Array.ofSeq (content |> Seq.choose parseLine) 
    //events |> Seq.map render |> Seq.iter (printfn "%s")
    let state = "", List.empty
    let withBoundaries = events |> Seq.fold assignBoundary state |> snd
    let byMod = withBoundaries |> Seq.groupBy fst
    let modules = byMod |> Seq.map (fun (m, evs) -> { Module = Path.GetFileName m; Events = Array.ofSeq (Seq.map snd evs) })
    modules |>
    Seq.iter printModuleStats


[<EntryPoint>]
let main argv = 
    match argv with 
    | [|fname|] -> 
        analyzeLog fname
    | _ -> 
        printfn "Specify one log file to analyze!"
    0