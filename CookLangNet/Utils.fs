[<AutoOpen>]
module internal Utils
let trim (s: string) = s.Trim()

open System.Runtime.CompilerServices

[<assembly: InternalsVisibleTo("CookLangNet.Tests")>]
do ()