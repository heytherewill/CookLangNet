[<AutoOpen>]
module internal Utils

let isNotNull x = x |> (isNull >> not)

let trim (s: string) = s.Trim()

open System.Runtime.CompilerServices

[<assembly: InternalsVisibleTo("CookLangNet.Tests")>]
do ()