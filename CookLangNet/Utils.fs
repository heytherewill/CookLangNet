[<AutoOpen>]
module internal Utils

open CookLangNet
open System

/// Helper for checking nullability
let isNotNull x = x |> (isNull >> not)

/// Removes leading and trailing whitespace from a string
let trim (s: string) = s.Trim()

/// Creates a quantity by trying to parse a number and defaulting to a raw string if it fails
let quantityFromString (quantity: string) =
    let mutable numberRef = ref 0.0
    
    // Tries parsing a fractioned quantity
    let parsedFraction =
        let symbolLocation = quantity.IndexOf("/")
        if (symbolLocation < 0) then None
        else
            let numerator = quantity.Substring(0, symbolLocation)
            let denominator = quantity.Substring(symbolLocation + 1, quantity.Length - symbolLocation - 1)
            if not (Double.TryParse(numerator, numberRef)) then None
            else
                let parsedNumerator = numberRef.Value
                if not (Double.TryParse(denominator, numberRef)) then None
                else
                    let parsedDenominator = numberRef.Value
                    if (parsedDenominator > 0) then Numeric (parsedNumerator / parsedDenominator) |> Some
                    else None

    match parsedFraction with
    | Some q -> q
    | None -> 
        if Double.TryParse(quantity, numberRef) then Numeric numberRef.Value
        else Textual (trim quantity)

/// Checks if a string is null or blank.
let isNullOrWhiteSpace (s: string) = System.String.IsNullOrWhiteSpace(s)

open System.Runtime.CompilerServices

[<assembly: InternalsVisibleTo("CookLangNet.Tests")>]
do ()