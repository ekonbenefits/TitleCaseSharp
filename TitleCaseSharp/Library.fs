namespace TitleCaseSharp

open System.Text.RegularExpressions
open System.Globalization
open System

(*
Original Perl version by: John Gruber http://daringfireball.net/ 10 May 2008
Python version by Stuart Colville http://muffinresearch.co.uk
F# version ported by James Tuley (Ekon Benefits) from python https://github.com/ppannuto/python-titlecase 2de89b2  
License: http://www.opensource.org/licenses/mit-license.php
*)

module TitleCase =

    let SMALL = @"a|an|and|as|at|but|by|en|for|if|in|of|on|or|the|to|v\.?|via|vs\.?"
    let PUNCT = """!"“#$%&'‘()*+,\-–‒—―./:;?@[\\\]_`{|}~"""
    let SMALL_WORDS = Regex($@"^(%s{SMALL})$", RegexOptions.IgnoreCase ||| RegexOptions.Compiled)
    let SMALL_FIRST = Regex($@"^([%s{PUNCT}]*)(%s{SMALL})\b", RegexOptions.IgnoreCase ||| RegexOptions.Compiled)
    let SMALL_LAST = Regex($@"\b(%s{SMALL})[%s{PUNCT}]?$", RegexOptions.IgnoreCase ||| RegexOptions.Compiled)
    let SUBPHRASE = Regex($@"([:.;?!\-–‒—―][ ])(%s{SMALL})", RegexOptions.Compiled)
    let MAC_MC = Regex(@"^([Mm]c|MC)(\w.+)", RegexOptions.Compiled)
    let MR_MRS_MS_DR = Regex(@"^((m((rs?)|s))|Dr)$", RegexOptions.IgnoreCase ||| RegexOptions.Compiled)
    let INLINE_PERIOD = Regex(@"[\w][.][\w]", RegexOptions.IgnoreCase ||| RegexOptions.Multiline ||| RegexOptions.Compiled)
    let UC_ELSEWHERE = Regex($@"[%s{PUNCT}]*?[a-zA-Z]+[A-Z]+?", RegexOptions.Compiled)
    let CAPFIRST = Regex($@"^[%s{PUNCT}]*?([\w])", RegexOptions.Compiled)
    let APOS_SECOND = Regex(@"^[dol]['‘][\w]+(?:['s]{2})?$", RegexOptions.IgnoreCase ||| RegexOptions.Compiled)
    let UC_INITIALS = Regex(@"^(?:[A-Z]\.|[A-Z]\.[A-Z])+$")
    let CONSONANTS = Regex($@"\A[%s{Set.difference (Set ['a'..'z']) (Set ['a';'e';'i';'o';'u';'y']) |> string}]+\Z",
                            RegexOptions.IgnoreCase ||| RegexOptions.Multiline ||| RegexOptions.Compiled)

    type PreProcessed = Mutable of string | Immutable of string

    [<AutoOpen>]
    module Patterns =
        let Preserve = Immutable >> Some
        let Accept = Mutable >> Some
        let capitalize = CultureInfo.CurrentCulture.TextInfo.ToTitleCase

        let (|Callback|_|) callback all_caps word =
            match callback with
            | Some(cb) -> word |> cb all_caps |> Preserve
            | _ -> None;

        let (|AllCaps|_|) word =
            if UC_INITIALS.IsMatch(word) then
                word |> Accept
            else
                None

        let (|AposSecond|_|) word =
            match APOS_SECOND.IsMatch(word) with
            | true -> 
                let casing =
                    if "aeiouAEIOU".Contains(string word[0]) then
                        Char.ToLower
                    else
                        Char.ToUpper
                seq { casing(word[0]); word[1]; Char.ToUpper(word[2]); yield! word[3..] } 
                |> string 
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
                seq { Char.ToUpper(word[0]); yield! word[1..]}
                |> string 
                |> Accept
            | false -> None

        let (|InlinePeriod|_|) all_caps word =
            if INLINE_PERIOD.IsMatch(word) || (not all_caps && UC_ELSEWHERE.IsMatch(word)) then
                word |> Accept
            else
                None
    
        let (|SmallWords|_|) word =
            if SMALL_WORDS.IsMatch(word) then
                word.ToLower() |> Accept
            else
                None

        let (|Slashes|_|) transf callback (word:string) =
            if word.Contains("/") &&  not <| word.Contains("//") then
                word.Split('/') 
                |> Seq.map(fun t -> t |> transf callback false ) 
                |> String.concat "/" |> Accept
            else
                None

        let (|Hyphens|_|) transf callback (word:string) =
            if word.Contains("-") then
                word.Split('-') 
                |> Seq.map(fun t -> t |> transf callback false ) 
                |> String.concat "-" |> Accept
            else
                None

        let (|Abbreviation|_|) all_caps (word:string) =
            let word' = if all_caps then word.ToLower() else word
            if CONSONANTS.IsMatch(word') && word.Length > 2 then
                word.ToUpper() |> Accept
            else
                None
  
    [<CompiledName("TransformWithCallback")>]
    let transformWithCallback (callback: Func<string, bool, string>) (text:string) =
        let rec transf callback small_first_last text  = 
            let processed = 
                seq {
                    let lines = Regex.Split(text, @"[\r\n]+");
                    for line in lines do
                        let all_caps = line.ToUpperInvariant() = line
                        let words = Regex.Split(line, @"[\t ]");
                        let tc_line =
                            [|
                                for word in words do
                                   match word with
                                   | Callback callback all_caps c -> c
                                   | AllCaps a -> a
                                   | AposSecond p -> p
                                   | MacMc transf callback mc -> mc
                                   | MrMrsMsDr mr -> mr
                                   | InlinePeriod all_caps ip -> ip
                                   | SmallWords sw -> sw
                                   | Slashes transf callback sl -> sl
                                   | Hyphens transf callback h -> h
                                   | Abbreviation all_caps ab -> ab
                                   | w -> CAPFIRST.Replace(w,fun m->m.Groups[0].Value.ToUpper()) |> Mutable
                            |]
                        let tc_line' =
                            if small_first_last then
                                seq {
                                    let last = tc_line.Length-1
                                    match tc_line[0] with 
                                    | Mutable s -> SMALL_FIRST.Replace(s, fun m -> $"%s{m.Groups[1].Value}%s{capitalize(m.Groups[2].Value)}") |> Mutable
                                    | x -> x
                                    yield! tc_line[1..last-1]
                                    match tc_line[last] with 
                                    | Mutable s -> SMALL_LAST.Replace(s, fun m -> capitalize(m.Groups[0].Value)) |> Mutable
                                    | x -> x
                            
                                }
                            else
                                tc_line
                        let result = tc_line' |> Seq.map (function | Mutable m -> m | Immutable i -> i) |> String.concat " "
                        SUBPHRASE.Replace(result, fun m -> capitalize(m.Groups[0].Value))
                }
            processed |> String.concat Environment.NewLine
            
        let callback' = 
            callback 
            |> Option.ofObj
            |> Option.map (fun f'-> (fun x y-> f'.Invoke(y, x)))
        text |> transf callback' true
    [<CompiledName("Transform")>]
    let transform = transformWithCallback null