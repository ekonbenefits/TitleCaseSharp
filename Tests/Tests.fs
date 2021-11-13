module Tests

open System
open Xunit
open TitleCaseSharp

[<Theory>]
[<InlineData("",
    "")>]
[<InlineData("word/word",
    "Word/Word")>]

[<InlineData("a title and/or string",
       "A Title and/or String")>]
[<InlineData("dance with me/let’s face the music and dance",
"Dance With Me/Let’s Face the Music and Dance")>]
[<InlineData("a-b end-to-end two-not-three/three-by-four/five-and",
"A-B End-to-End Two-Not-Three/Three-by-Four/Five-And")>]
[<InlineData("34th 3rd 2nd",
       "34th 3rd 2nd")>]
[<InlineData("Q&A with steve jobs: 'that's what happens in technology'",
      "Q&A With Steve Jobs: 'That's What Happens in Technology'")>]
[<InlineData("What is AT&T's problem?",
    "What Is AT&T's Problem?")>]
[<InlineData("Apple deal with AT&T falls through",
   "Apple Deal With AT&T Falls Through")>]
[<InlineData("Words with all consonants like cnn are acronyms",
"Words With All Consonants Like CNN Are Acronyms")>]
[<InlineData("this v that",
  "This v That")>]
[<InlineData("this v. that",
    "This v. That")>]
[<InlineData("this vs that",
"This vs That")>]
[<InlineData("this vs. that",
 "This vs. That")>]
[<InlineData("The SEC's Apple probe: what you need to know",
      "The SEC's Apple Probe: What You Need to Know")>]
[<InlineData("'by the Way, small word at the start but within quotes.'",
     "'By the Way, Small Word at the Start but Within Quotes.'")>]
[<InlineData("Small word at end is nothing to be afraid of",
    "Small Word at End Is Nothing to Be Afraid Of")>]
[<InlineData("Starting Sub-Phrase With a Small Word: a Trick, Perhaps?",
"Starting Sub-Phrase With a Small Word: A Trick, Perhaps?")>]
[<InlineData("Sub-Phrase With a Small Word in Quotes: 'a Trick, Perhaps?'",
"Sub-Phrase With a Small Word in Quotes: 'A Trick, Perhaps?'")>]
[<InlineData("sub-phrase with a small word in quotes: \"a trick, perhaps?\"",
  "Sub-Phrase With a Small Word in Quotes: \"A Trick, Perhaps?\"")>]
[<InlineData("Starting a Hyphen Delimited Sub-Phrase With a Small Word - a Trick, Perhaps?",
    "Starting a Hyphen Delimited Sub-Phrase With a Small Word - A Trick, Perhaps?")>]
[<InlineData("Starting a Hyphen Delimited Sub-Phrase With a Small Word - a Trick, Perhaps?",
    "Starting a Hyphen Delimited Sub-Phrase With a Small Word - A Trick, Perhaps?")>]
[<InlineData("Hyphen Delimited sub-phrase with a small word in quotes - \"a trick, perhaps?\"",
"Hyphen Delimited Sub-Phrase With a Small Word in Quotes - \"A Trick, Perhaps?\"")>]
[<InlineData("Snakes on a Plane - The TV Edit - The Famous Line",
"Snakes on a Plane - The TV Edit - The Famous Line")>]
[<InlineData("Starting an Em Dash Delimited Sub-Phrase With a Small Word — a Trick, Perhaps?",
"Starting an Em Dash Delimited Sub-Phrase With a Small Word — A Trick, Perhaps?")>]
[<InlineData("Em Dash Delimited Sub-Phrase With a Small Word in Quotes — 'a Trick, Perhaps?'",
 "Em Dash Delimited Sub-Phrase With a Small Word in Quotes — 'A Trick, Perhaps?'")>]
[<InlineData("Em Dash Delimited sub-phrase with a small word in quotes — \"a trick, perhaps?\"",
     "Em Dash Delimited Sub-Phrase With a Small Word in Quotes — \"A Trick, Perhaps?\"")>]
[<InlineData("Snakes on a Plane — The TV Edit — The Famous Line",
       "Snakes on a Plane — The TV Edit — The Famous Line")>]
[<InlineData("EPISODE 7 — THE FORCE AWAKENS",
"Episode 7 — The Force Awakens")>]
[<InlineData("episode 7 – The force awakens",
"Episode 7 – The Force Awakens")>]
[<InlineData("THE CASE OF X ≤ 7",
"The Case of X ≤ 7")>]
[<InlineData("the case of X ≤ 7",
      "The Case of X ≤ 7")>]
[<InlineData("\"Nothing to Be Afraid of?\"",
   "\"Nothing to Be Afraid Of?\"")>]
[<InlineData("\"Nothing to be Afraid Of?\"",
"\"Nothing to Be Afraid Of?\"")>]
[<InlineData("a thing",
       "A Thing")>]
[<InlineData("2lmc Spool: 'gruber on OmniFocus and vapo(u)rware'",
   "2lmc Spool: 'Gruber on OmniFocus and Vapo(u)rware'")>]
[<InlineData("this is just an example.com",
"This Is Just an example.com")>]
[<InlineData("this is something listed on del.icio.us",
"This Is Something Listed on del.icio.us")>]
[<InlineData("iTunes should be unmolested",
    "iTunes Should Be Unmolested")>]
[<InlineData("reading between the lines of steve jobs’s ‘thoughts on music’",
   "Reading Between the Lines of Steve Jobs’s ‘Thoughts on Music’")>]
[<InlineData("seriously, ‘repair permissions’ is voodoo",
     "Seriously, ‘Repair Permissions’ Is Voodoo")>]
[<InlineData("generalissimo francisco franco: still dead; kieren McCarthy: still a jackass",
"Generalissimo Francisco Franco: Still Dead; Kieren McCarthy: Still a Jackass")>]
[<InlineData("O'Reilly should be untouched",
      "O'Reilly Should Be Untouched")>]
[<InlineData("my name is o'reilly",
  "My Name Is O'Reilly")>]
[<InlineData("WASHINGTON, D.C. SHOULD BE FIXED BUT MIGHT BE A PROBLEM",
 "Washington, D.C. Should Be Fixed but Might Be a Problem")>]
[<InlineData("THIS IS ALL CAPS AND SHOULD BE ADDRESSED",
"This Is All Caps and Should Be Addressed")>]
[<InlineData("Mr McTavish went to MacDonalds",
"Mr McTavish Went to MacDonalds")>]
[<InlineData("this is http://foo.com",
 "This Is http://foo.com")>]
[<InlineData("mac mc MAC MC machine",
      "Mac Mc MAC MC Machine")>]
[<InlineData("FOO BAR 5TH ST",
       "Foo Bar 5th St")>]
[<InlineData("foo bar 5th st",
    "Foo Bar 5th St")>]
[<InlineData("l'grange l'grange l'Grange l'Grange",
 "l'Grange l'Grange l'Grange l'Grange")>]
[<InlineData("L'grange L'grange L'Grange L'Grange",
      "l'Grange l'Grange l'Grange l'Grange")>]
[<InlineData("l'GranGe",
   "l'GranGe")>]
[<InlineData("o'grange O'grange o'Grange O'Grange",
   "O'Grange O'Grange O'Grange O'Grange")>]
[<InlineData("o'grange's O'grange's o'Grange's O'Grange's",
    "O'Grange's O'Grange's O'Grange's O'Grange's")>]
[<InlineData("O'GranGe",
     "O'GranGe")>]
[<InlineData("o'melveny/o'doyle o'Melveny/o'doyle O'melveny/o'doyle o'melveny/o'Doyle o'melveny/O'doyle",
"O'Melveny/O'Doyle O'Melveny/O'Doyle O'Melveny/O'Doyle O'Melveny/O'Doyle O'Melveny/O'Doyle")>]
// These 'Mc' cases aim to ensure more consistent/predictable behavior.
// The examples here are somewhat contrived, and are subject to change
// if there is a compelling argument for updating their behavior.
// See https://github.com/ppannuto/python-titlecase/issues/64
[<InlineData("mccay-mcbut-mcdo mcdonalds/mcby",
       "McCay-McBut-McDo McDonalds/McBy")>]
[<InlineData("oblon, spivak, mcclelland, maier & neustadt",
     "Oblon, Spivak, McClelland, Maier & Neustadt")>]
[<InlineData("Mcoblon, spivak, mcclelland, mcmaier, & mcneustadt",
 "McOblon, Spivak, McClelland, McMaier, & McNeustadt")>]
[<InlineData("mcfoo-bar, MCFOO-BAR, McFoo-bar, McFoo-Bar, mcfoo-mcbar, foo-mcbar",
"McFoo-Bar, McFoo-Bar, McFoo-Bar, McFoo-Bar, McFoo-McBar, Foo-McBar")>]
[<InlineData("'QUOTE' A GREAT",
    "'Quote' a Great")>]
[<InlineData("‘QUOTE’ A GREAT",
      "‘Quote’ a Great")>]
[<InlineData("“YOUNG AND RESTLESS”",
"“Young and Restless”")>]
[<InlineData("EL NIÑO A ARRIVÉ HIER",
   "El Niño a Arrivé Hier")>]
[<InlineData("YEA NO",
       "Yea No")>]
[<InlineData("ÝÆ ÑØ",
   "Ýæ Ñø")>]
[<InlineData("yea no",
     "Yea No")>]
[<InlineData( "ýæ ñø",
"Ýæ Ñø")>]
//https://github.com/ppannuto/python-titlecase/pull/67
[<InlineData("Mr mr Mrs Ms Mss Dr dr , Mr. and Mrs. Person",
"Mr Mr Mrs Ms MSS Dr Dr , Mr. And Mrs. Person")>]
//EkonBenefits
[<InlineData("O'BANNON,", "O'Bannon,")>]
[<InlineData("O'BANNON, ROCKNE", "O'Bannon, Rockne")>]
let ``Generated Tests`` (input:string,  expected:string) =
    Assert.Equal(expected, input |> String.titleCase)

[<Fact>]
let ``Test line mangle normalized to pplatform line ending`` () =
    let input ="this shouldn't\nget mangled"
    let expected = $"This Shouldn't{Environment.NewLine}Get Mangled"
    Assert.Equal(expected, input |> String.titleCase)


[<Fact>]
let ``Test Callback`` () =
    let abbreviation (word:TitleCaseWord) =
        if Set ["TCP"; "UDP"] |> Set.contains (word.Word.ToUpperInvariant()) then
            Some <| word.Word.ToUpperInvariant()
        else
            None
    let s = "a simple tcp and udp wrapper"
    // Note: this library is able to guess that all-consonant words are acronyms, so TCP
    // works naturally, but others will require the custom list
    Assert.Equal("A Simple TCP and Udp Wrapper", s |> String.titleCase)
    Assert.Equal("A Simple TCP and UDP Wrapper", s |> String.titleCaseWith abbreviation)
    Assert.Equal("A Simple TCP and UDP Wrapper", s.ToUpper() |> String.titleCaseWith abbreviation)
    Assert.Equal("CRÈME BRÛLÉE", "crème brûlée" |> String.titleCaseWith (fun w -> Some <| w.Word.ToUpperInvariant()))

[<Fact>]
let ``Test AT&T`` () =
    Assert.Equal("AT&T", "at&t" |> String.titleCaseWith (function | {Word=x} when x.ToUpperInvariant() = "AT&T" -> Some <| x.ToUpperInvariant() | _ -> None))