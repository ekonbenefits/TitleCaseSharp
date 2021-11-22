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
type TitleCaseParsedWord = {NonAlphaPrefix:string; CoreWord:string; NonAlphaSuffix:string}
type TitleCaseWord = {FullCapture:string; Word:TitleCaseParsedWord; LineState: TitleCaseLineState}


module Internals =

    [<AutoOpen>]
    module Regex =
        let SMALL = @"a|an|and|as|at|but|by|en|for|if|in|of|on|or|the|to|v\.?|via|vs\.?"
        let PUNCT = """!"“#$%&'‘()*+,\-–‒—―.\/:;?@[\\\]_`{|}~"""
        let SMALL_WORDS = Regex(sprintf @"^(%s)$" SMALL, RegexOptions.IgnoreCase ||| RegexOptions.Compiled)
        let SMALL_FIRST = Regex(sprintf @"^([%s]*)(%s)\b" PUNCT SMALL, RegexOptions.IgnoreCase ||| RegexOptions.Compiled)
        let SMALL_LAST = Regex(sprintf @"\b(%s)[%s]?$" SMALL PUNCT, RegexOptions.IgnoreCase ||| RegexOptions.Compiled)
        let SUBPHRASE = Regex(sprintf @"((?:[a-zA-Z]{2,}|[^a-zA-Z]+)[:.;?!\-–‒—―][ ])(%s)"SMALL, RegexOptions.Compiled)
        let MAC_MC = Regex(@"^([Mm]c|MC)(\w.+)", RegexOptions.Compiled)
        let MR_MRS_MS_DR = "((m((rs?)|s))|Dr)"
        let MR_MRS_MS_DR_CAP = Regex(sprintf @"^%s$" MR_MRS_MS_DR, RegexOptions.IgnoreCase ||| RegexOptions.Compiled)
        let MR_MRS_MS_DR_2ND = Regex(sprintf @"^%s\.\s+$" MR_MRS_MS_DR, RegexOptions.IgnoreCase ||| RegexOptions.Compiled)
        let INLINE_PERIOD = Regex(@"[\w][.][\w]", RegexOptions.IgnoreCase ||| RegexOptions.Multiline ||| RegexOptions.Compiled)
        let UC_ELSEWHERE = Regex(sprintf @"[%s]*?[a-zA-Z]+[A-Z]+?" PUNCT, RegexOptions.Compiled)
        let CAPFIRST = Regex(sprintf @"^[%s]*?([\w])" PUNCT, RegexOptions.Compiled)
        let APOS_SECOND = Regex(sprintf @"^[dolj]['‘][\w]+(?:['s]{2})?[%s]?$" PUNCT, RegexOptions.IgnoreCase ||| RegexOptions.Compiled)
        let UC_INITIALS = Regex(@"^(?:[A-Z]\.|[A-Z]\.[A-Z])+$")
        let CONSONANTS = Regex(sprintf @"\A[%s]+\Z" (Set.difference (Set ['a'..'z']) (Set ['a';'e';'i';'o';'u';'y']) |> Seq.toArray |> String),
                                RegexOptions.IgnoreCase ||| RegexOptions.Multiline ||| RegexOptions.Compiled)
        let CORE_WORD = Regex(@"^([^a-zA-Z]*)([a-zA-Z]+.*[a-zA-Z]+|[a-zA-Z]+)([^a-zA-Z]*)$");

    type PreProcessed = Mutable of string | Immutable of string

    [<AutoOpen>]
    module Patterns =
        let Preserve = Immutable >> Some
        let Accept = Mutable >> Some
        let capitalizeFirstLetter (word:string) = CAPFIRST.Replace(word.ToLowerInvariant(),fun m->m.Groups.[0].Value.ToUpperInvariant())
        let (|Callback|_|) callback lineState word =
            match callback with
            | Some(cb) ->
                let m = CORE_WORD.Match(word)
                let cw = 
                    if m.Success then
                        { NonAlphaPrefix = m.Groups.[1].Value; CoreWord = m.Groups.[2].Value; NonAlphaSuffix = m.Groups.[3].Value }
                    else 
                        {NonAlphaPrefix = ""; CoreWord = word; NonAlphaSuffix = ""}

                {FullCapture = word; Word = cw; LineState =lineState} |> cb |> Option.bind Preserve
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
 //                   Original only capitalized vowels, assumed d' and l' were used in non american context
 //                   However, american proper names like D'Andre are capitalized, this title case library is special
 //                   cased to american english so i'm not going to keep the exception.
                        Char.ToUpperInvariant
                let tailCasing =
                    if state.AllCaps then
                        Char.ToLowerInvariant
                    else
                        id
                [| casing(word.[0]); word.[1]; Char.ToUpperInvariant(word.[2]); yield! word.[3..] |> Seq.map tailCasing |]
                |> String 
                |> Accept
            | false  -> None
        let (|MacMc|_|) transf word =
            match MAC_MC.Match(word) with
            | m when m.Success ->
                sprintf "%s%s" (capitalizeFirstLetter m.Groups.[1].Value) (m.Groups.[2].Value |> transf true) |> Accept
            | _ -> None
        let (|MrMrsMsDr|_|) word =
            match MR_MRS_MS_DR_CAP.IsMatch(word) with
            | true -> 
                [|Char.ToUpperInvariant(word.[0]); yield! word.[1..] |]
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
        let (|Slashes|_|) transf (word:string) =
            if word.Contains("/") &&  not <| word.Contains("//") then
                word.Split('/') 
                |> Seq.map(transf false) 
                |> String.concat "/"
                |> Accept
            else
                None
        let (|Hyphens|_|) transf (word:string) =
            if word.Contains("-") then
                word.Split('-') 
                |> Seq.map(transf false) 
                |> String.concat "-"
                |> Accept
            else
                None
        let (|Pipes|_|) transf (word:string) =
                if word.Contains("|") then
                    word.Split('|') 
                    |> Seq.map(transf false) 
                    |> String.concat "|"
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
                        CAPFIRST.Replace(w',fun m->m.Groups.[0].Value.ToUpperInvariant()) |> Mutable

                    let recurseFunction = titleCaseTransformer callback

                    let tc_line =
                        [|
                            for word in words do
                                match word with
                                | Callback callback lineState c -> c
                                | AllCaps a -> a
                                | AposSecond lineState p -> p
                                | MacMc recurseFunction mc -> mc
                                | MrMrsMsDr mr -> mr
                                | InlinePeriod lineState ip -> ip
                                | SmallWords sw -> sw
                                | Slashes recurseFunction sl -> sl
                                | Hyphens recurseFunction h -> h
                                | Pipes recurseFunction pp -> pp
                                | Abbreviation lineState ab -> ab
                                | w -> basicCapFirs w
                        |]
                    if small_first_last then
                        let last = tc_line.Length-1
                        let p0 =
                            match tc_line.[0] with 
                            | Mutable s -> SMALL_FIRST.Replace(s, fun m -> sprintf "%s%s" m.Groups.[1].Value (capitalizeFirstLetter m.Groups.[2].Value)) |> Mutable
                            | x -> x
                        p0 |> Array.set tc_line 0 
                        let pLast = 
                            match tc_line.[last] with 
                            | Mutable s -> SMALL_LAST.Replace(s, fun m -> capitalizeFirstLetter(m.Groups.[0].Value)) |> Mutable
                            | x -> x
                        pLast |> Array.set tc_line last 
                    let result = tc_line |> Seq.map (function | Mutable m -> m | Immutable i -> i) |> String.concat " "

                    let subReplace (m:Match) = 
                        let wordPrePunc = m.Groups.[1].Value
                        let casing =
                            if MR_MRS_MS_DR_2ND.IsMatch(wordPrePunc) then
                                id
                            else 
                                capitalizeFirstLetter
                        sprintf "%s%s" wordPrePunc (casing m.Groups.[2].Value)

                    SUBPHRASE.Replace(result, MatchEvaluator subReplace)
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
