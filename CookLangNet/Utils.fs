[<AutoOpen>]
module internal Utils

/// Helper for checking nullability
let isNotNull x = x |> (isNull >> not)

/// Removes leading and trailing whitespace from a string
let trim (s: string) = s.Trim()

open System.Runtime.CompilerServices

[<assembly: InternalsVisibleTo("CookLangNet.Tests")>]
do ()