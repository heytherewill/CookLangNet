namespace CookLangNet

open FParsec
    
module internal Parser =
    // Intermediate state parser models
    type ParsedStepDecorations = 
        | ParsedIngredient of Ingredient
        | ParsedEquipment of Equipment
        | ParsedTimer of Timer
        | InlineComment  of string

    type ParsedLine =
        | Comment of string
        | Metadata of string * string
        | Step of Step

    // Helper parsers
    let many1CharsExceptThese chars = many1Chars (noneOf chars)
    let manyCharsExcept char = manyChars (noneOf [ char ])
    let anyCharsTillParser p = manyCharsTill anyChar p
    let anyCharsTillChar c = anyCharsTillParser (pchar c)
    let anyCharsTillString s = anyCharsTillParser (pstring s)
    let anyCharsTillSpace = anyCharsTillParser spaces1 

    // Helper functions
    let trim (s: string) = s.Trim()
        
    // Type aliases
    let decorationChars = ['#'; '@'; '~' ; '/']

    // User State manipulation
    let addDecoration d = updateUserState (fun decorations -> d::decorations)
    let clearUserState s = updateUserState (fun _ -> []) >>% s

    // Comments
    let comment = skipString "//" >>. restOfLine true |>> trim
    let commentLine = comment |>> ParsedLine.Comment
    let inlineComment = comment |>> InlineComment |>> addDecoration >>% ""

    // Metadata
    let metadataFromStringParts (list: string list) =
        let key = list.Head.Trim()
        let value = list.Tail |> String.concat ":" |> trim
        Metadata(key, value)

    let metadata =
        skipString ">>"
        >>. sepBy1 (manyCharsExcept ':') (pchar ':') 
        |>> metadataFromStringParts

    // Ingredients
    let private toIngredientAmount (quantity, unit) = 
        { Quantity = quantity; Unit = unit }

    let private addComplexIngredientDecoration (name, amount) =
        let ingredient = ParsedIngredient { Name = name; Amount = amount }
        addDecoration ingredient >>% name

    let private addSimpleIngredientDecoration name =
        addComplexIngredientDecoration (name, None)

    let simpleIngredient        = anyCharsTillSpace >>= addSimpleIngredientDecoration
    let complexIngredientName   = anyCharsTillChar '{'
    let complexIngredientAmount = pfloat .>>. (opt (skipChar '%' >>. (manyCharsExcept '}'))) |>> toIngredientAmount
    let complexIngredient = (complexIngredientName .>>. (opt complexIngredientAmount) .>> skipChar '}') >>= addComplexIngredientDecoration
    let ingredient = skipChar '@' >>. ((attempt complexIngredient) <|> simpleIngredient)

    // Equipment
    let private addEquipmentDecoration name =
        let equipment = ParsedEquipment { Name = name }
        addDecoration equipment >>% name

    let private simpleEquipment  = anyCharsTillSpace       >>= addEquipmentDecoration
    let private complexEquipment = anyCharsTillString "{}" >>= addEquipmentDecoration
    let equipment = skipChar '#' >>. ((attempt complexEquipment) <|> simpleEquipment)

    // Timer
    let private addTimerDecoration (duration, unit) =
        let timer = ParsedTimer { Duration = duration; Unit = unit}
        addDecoration timer >>% (duration.ToString() + " " + unit)

    let timer = skipString "~{" >>. (pfloat .>>. (skipChar '%' >>. (manyCharsExcept '}'))) >>= addTimerDecoration

    // Steps
    let convertStateToStep (directions, decorations) =
        match decorations with
        | [] -> Step { Directions = directions; Timers = []; Ingredients = []; NeededEquipment = []; Comment = "" }
        | _ ->
            let actualDecorations = decorations |> List.rev
            Step {
                Directions = directions
                Timers = actualDecorations |> List.choose (function ParsedTimer t -> Some t | _ -> None)
                Ingredients = actualDecorations |> List.choose (function ParsedIngredient i -> Some i | _ -> None)
                NeededEquipment = actualDecorations |> List.choose (function ParsedEquipment e -> Some e | _ -> None)
                Comment = "" //decorations |> Seq.choose (function InlineComment c -> Some c | _ -> None) |> Seq.tryHead ?? ""
            }

    let parseStepUntilDecoration = many1CharsExceptThese decorationChars
    let parseDecoration = choice [ ingredient; equipment ; timer ; inlineComment ]

    let stepDirections = (many (parseDecoration <|> parseStepUntilDecoration)) |>> String.concat ""
    let step = (stepDirections .>>. getUserState) |>> convertStateToStep >>= clearUserState
    
    // Line parser
    let line = choice [ commentLine ; metadata ; step ]