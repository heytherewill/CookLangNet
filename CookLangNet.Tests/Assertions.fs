[<AutoOpen>]
module Assertions

open CookLangNet
open FsUnit.Xunit
open System.Collections.Generic

let shouldBeTheSameRecipeAs expectedRecipe actualRecipe =
    let compareMetadata (actual: KeyValuePair<string, string>, expected: KeyValuePair<string, string>) =
        actual.Key |> should equal expected.Key
        actual.Value |> should equal expected.Value
        ()

    actualRecipe.Steps |> should equal expectedRecipe.Steps
    // Comparing dictionaries doesn't work, so we need to check each entry instead.
    actualRecipe.Metadata
    |> Seq.zip expectedRecipe.Metadata
    |> Seq.iter compareMetadata
