# TitleCaseSharp

.net port of titlecase https://github.com/ppannuto/python-titlecase

![CI build](https://github.com/ekonbenefits/TitleCaseSharp/actions/workflows/dotnet.yml/badge.svg)

## C#
```csharp
TitleCase.Transform("The SEC's Apple probe: what you need to know"); //The SEC's Apple Probe: What You Need to Know
Titlecase.Transform("dance with me/let’s face the music and dance"); //Dance With Me/Let’s Face the Music and Dance
TitleCase.Transform("Mr McTavish went to MacDonalds"); //Mr McTavish Went to MacDonalds
```
## F#
```fsharp
"The SEC's Apple probe: what you need to know" |> TitleCase.transform //The SEC's Apple Probe: What You Need to Know
"dance with me/let’s face the music and dance" |> TitleCase.transform //Dance With Me/Let’s Face the Music and Dance
"Mr McTavish went to MacDonalds") |> TitleCase.transform //Mr McTavish Went to MacDonalds
```
