namespace TitleCaseSharp

open System.Text.RegularExpressions
open System

(*
Original Perl version by: John Gruber http://daringfireball.net/ 10 May 2008
Python version by Stuart Colville http://muffinresearch.co.uk
F# version ported by James Tuley 2021 (Ekon Benefits) from python https://github.com/ppannuto/python-titlecase 2de89b2  
License: http://www.opensource.org/licenses/mit-license.php
*)

type TitleCaseLineState = { AllCaps: bool}
type TitleCaseWord = {Word:string; LineState: TitleCaseLineState}

module Internals =

    [<AutoOpen>]
    module Regex =
        let SMALL = @"a|an|and|as|at|but|by|en|for|if|in|of|on|or|the|to|v\.?|via|vs\.?"
        let PUNCT = """!"“#$%&'‘()*+,\-–‒—―.\/:;?@[\\\]_`{|}~"""
        let SMALL_WORDS = Regex($@"^(%s{SMALL})$", RegexOptions.IgnoreCase ||| RegexOptions.Compiled)
        let SMALL_FIRST = Regex($@"^([%s{PUNCT}]*)(%s{SMALL})\b", RegexOptions.IgnoreCase ||| RegexOptions.Compiled)
        let SMALL_LAST = Regex($@"\b(%s{SMALL})[%s{PUNCT}]?$", RegexOptions.IgnoreCase ||| RegexOptions.Compiled)
        let SUBPHRASE = Regex($@"([:.;?!\-–‒—―][ ])(%s{SMALL})", RegexOptions.Compiled)
        let MAC_MC = Regex(@"^([Mm]c|MC)(\w.+)", RegexOptions.Compiled)
        let MR_MRS_MS_DR = Regex(@"^((m((rs?)|s))|Dr)$", RegexOptions.IgnoreCase ||| RegexOptions.Compiled)
        let INLINE_PERIOD = Regex(@"[\w][.][\w]", RegexOptions.IgnoreCase ||| RegexOptions.Multiline ||| RegexOptions.Compiled)
        let UC_ELSEWHERE = Regex($@"[%s{PUNCT}]*?[a-zA-Z]+[A-Z]+?", RegexOptions.Compiled)
        let CAPFIRST = Regex($@"^[%s{PUNCT}]*?([\w])", RegexOptions.Compiled)
        let APOS_SECOND = Regex($@"^[dol]['‘][\w]+(?:['s]{{2}})?[%s{PUNCT}]?$", RegexOptions.IgnoreCase ||| RegexOptions.Compiled)
        let UC_INITIALS = Regex(@"^(?:[A-Z]\.|[A-Z]\.[A-Z])+$")
        let CONSONANTS = Regex($@"\A[%s{Set.difference (Set ['a'..'z']) (Set ['a';'e';'i';'o';'u';'y']) |> Seq.toArray |> String}]+\Z",
                                RegexOptions.IgnoreCase ||| RegexOptions.Multiline ||| RegexOptions.Compiled)

    type PreProcessed = Mutable of string | Immutable of string

    [<AutoOpen>]
    module Patterns =
        let Preserve = Immutable >> Some
        let Accept = Mutable >> Some
        let capitalize (word:string) = CAPFIRST.Replace(word.ToLowerInvariant(),fun m->m.Groups[0].Value.ToUpperInvariant())
        let (|Callback|_|) callback lineState word =
            match callback with
            | Some(cb) -> {Word = word; LineState =lineState} |> cb |> Option.bind Preserve
            | _ -> None;
        let (|AllCaps|_|) word =
            if UC_INITIALS.IsMatch(word) then
                word |> Accept
            else
                None
        let (|AposSecond|_|) state word =
            match APOS_SECOND.IsMatch(word) with
            | true -> 
                let casing =
                    if not <| "aeiouAEIOU".Contains(string word[0]) then
                        Char.ToLowerInvariant
                    else
                        Char.ToUpperInvariant
                let tailCasing =
                    if state.AllCaps then
                        Char.ToLowerInvariant
                    else
                        id
                [| casing(word[0]); word[1]; Char.ToUpperInvariant(word[2]); yield! word[3..] |> Seq.map tailCasing |]
                |> String 
                |> Accept
            | false  -> None
        let (|MacMc|_|) transf callback word =
            match MAC_MC.Match(word) with
            | m when m.Success ->
                $"%s{capitalize(m.Groups[1].Value)}%s{m.Groups[2].Value |> transf callback true}" |> Accept
            | _ -> None
        let (|MrMrsMsDr|_|) word =
            match MR_MRS_MS_DR.IsMatch(word) with
            | true -> 
                [|Char.ToUpperInvariant(word[0]); yield! word[1..] |]
                |> String 
                |> Accept
            | false -> None
        let (|InlinePeriod|_|) lineState word =
            if INLINE_PERIOD.IsMatch(word) || (not lineState.AllCaps && UC_ELSEWHERE.IsMatch(word)) then
                word |> Accept
            else
                None
        let (|SmallWords|_|) word =
            if SMALL_WORDS.IsMatch(word) then
                word.ToLowerInvariant() |> Accept
            else
                None
        let (|Slashes|_|) transf callback (word:string) =
            if word.Contains("/") &&  not <| word.Contains("//") then
                word.Split('/') 
                |> Seq.map(transf callback false) 
                |> String.concat "/"
                |> Accept
            else
                None
        let (|Hyphens|_|) transf callback (word:string) =
            if word.Contains("-") then
                word.Split('-') 
                |> Seq.map(transf callback false) 
                |> String.concat "-"
                |> Accept
            else
                None
        let (|Abbreviation|_|) lineState (word:string) =
            let word' = if lineState.AllCaps then word.ToLowerInvariant() else word
            if CONSONANTS.IsMatch(word') && word.Length > 2 then
                word.ToUpperInvariant() |> Accept
            else
                None
    
    let rec titleCaseTransformer callback small_first_last text = 
        let processed = 
            seq {
                let lines = Regex.Split(text, @"[\r\n]+");
                for line in lines do
                    let lineState = {AllCaps=line.ToUpperInvariant() = line} 
                    let words = Regex.Split(line, @"[\t ]");
                    let basicCapFirs (w:string) = 
                        let w' = if lineState.AllCaps then w.ToLowerInvariant() else w
                        CAPFIRST.Replace(w',fun m->m.Groups[0].Value.ToUpperInvariant()) |> Mutable
                    let tc_line =
                        [|
                            for word in words do
                                match word with
                                | Callback callback lineState c -> c
                                | AllCaps a -> a
                                | AposSecond lineState p -> p
                                | MacMc titleCaseTransformer callback mc -> mc
                                | MrMrsMsDr mr -> mr
                                | InlinePeriod lineState ip -> ip
                                | SmallWords sw -> sw
                                | Slashes titleCaseTransformer callback sl -> sl
                                | Hyphens titleCaseTransformer callback h -> h
                                | Abbreviation lineState ab -> ab
                                | w -> basicCapFirs w
                        |]
                    let tc_line' =
                        if small_first_last then
                            let last = tc_line.Length-1
                            let p0 =
                                match tc_line[0] with 
                                | Mutable s -> SMALL_FIRST.Replace(s, fun m -> $"%s{m.Groups[1].Value}%s{capitalize(m.Groups[2].Value)}") |> Mutable
                                | x -> x
                            let tc0 = tc_line |> Array.updateAt 0 p0
                            let pLast = 
                                match tc0[last] with 
                                | Mutable s -> SMALL_LAST.Replace(s, fun m -> capitalize(m.Groups[0].Value)) |> Mutable
                                | x -> x
                            tc0 |> Array.updateAt last pLast
                        else
                            tc_line
                    let result = tc_line' |> Seq.map (function | Mutable m -> m | Immutable i -> i) |> String.concat " "
                    SUBPHRASE.Replace(result, fun m ->  $"%s{m.Groups[1].Value}%s{capitalize(m.Groups[2].Value)}")
            }
        processed |> String.concat Environment.NewLine

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module String =
    [<CompiledName("TitleCase")>]
    let titleCase =  Internals.titleCaseTransformer None true
    [<CompiledName("TitleCaseWith")>]
    let titleCaseWith callback = Internals.titleCaseTransformer (Some callback) true

open System.Runtime.CompilerServices
open System.Runtime.InteropServices
[<Extension>]
type Transform = 
    [<Extension>]
    static member ToTitleCase(text:string,
        [<Optional; DefaultParameterValue(null:Func<TitleCaseWord, string>)>] 
        callback:Func<TitleCaseWord, string>) =
            let callback' = 
                callback 
                |> Option.ofObj
                |> Option.map (fun f'-> (fun x-> f'.Invoke(x) |> Option.ofObj))
            text |> Internals.titleCaseTransformer callback' true
