[<AutoOpen>]
module internal Utils

/// Helper for checking nullability
let isNotNull x = x |> (isNull >> not)

/// Removes leading and trailing whitespace from a string
let trim (s: string) = s.Trim()

/// Checks if a string is null or blank.
let isNullOrWhiteSpace (s: string) = System.String.IsNullOrWhiteSpace(s)

open System.Runtime.CompilerServices

[<assembly: InternalsVisibleTo("CookLangNet.Tests")>]
do ()