# TitleCaseSharp

.net port of titlecase https://github.com/ppannuto/python-titlecase

![CI build](https://github.com/ekonbenefits/TitleCaseSharp/actions/workflows/dotnet.yml/badge.svg)

## C#
```csharp
using TitleCaseSharp;

"The SEC's Apple probe: what you need to know".ToTitleCase(); //The SEC's Apple Probe: What You Need to Know
"dance with me/let’s face the music and dance".ToTitleCase(); //Dance With Me/Let’s Face the Music and Dance
"Mr McTavish went to MacDonalds".ToTitleCase(); //Mr McTavish Went to MacDonalds
```
## F#
```fsharp
open TitleCaseSharp

"The SEC's Apple probe: what you need to know" |> string.titleCase //The SEC's Apple Probe: What You Need to Know
"dance with me/let’s face the music and dance" |> string.titleCase //Dance With Me/Let’s Face the Music and Dance
"Mr McTavish went to MacDonalds" |> string.titleCase //Mr McTavish Went to MacDonalds

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
```
