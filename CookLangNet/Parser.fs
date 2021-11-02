namespace CookLangNet

open FParsec
    
module internal Parser =
    // Intermediate state parser models
    type State = {
        ParsedIngredients: Ingredient list
        ParsedEquipment: Equipment list
        ParsedTimers: Timer list
        ParsedComment: string option
    }
    with 
        static member Empty = { 
            ParsedIngredients = []
            ParsedEquipment = []
            ParsedTimers = []
            ParsedComment = None
        }

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
    let manyCharsExceptWordDelimiters = manyChars (noneOf [ ',' ; '.' ; '\n'; '\r'; ' ' ])
    let anyButDecorationDelimiters = noneOf "@#~"

    // User State manipulation
    let addComment c = updateUserState (fun s -> { s with ParsedComment = Some c })
    let addIngredient i = updateUserState (fun s -> { s with ParsedIngredients = i::s.ParsedIngredients })
    let addEquipment e = updateUserState (fun s -> { s with ParsedEquipment = e::s.ParsedEquipment })
    let addTimer t = updateUserState (fun s -> { s with ParsedTimers = t::s.ParsedTimers })
    let clearUserState s = updateUserState (fun _ -> State.Empty) >>% s

    // Comments
    let comment = skipString "//" >>. restOfLine true |>> trim
    let commentLine = comment |>> ParsedLine.Comment
    let inlineComment = comment >>= addComment >>% ""

    // Metadata
    let metadataFromStringParts (list: string list) =
        let key = list.Head |> trim
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
        let ingredient = { Name = name; Amount = amount }
        addIngredient ingredient >>% name

    let private addSimpleIngredientDecoration name =
        addComplexIngredientDecoration (name, None)

    let simpleIngredient        = manyCharsExceptWordDelimiters >>= addSimpleIngredientDecoration
    let complexIngredientName   = manyCharsTill anyButDecorationDelimiters (pchar '{')
    let complexIngredientAmount = pfloat .>>. (opt (skipChar '%' >>. (manyCharsExcept '}'))) |>> toIngredientAmount
    let complexIngredient = (complexIngredientName .>>. (opt complexIngredientAmount) .>> skipChar '}') >>= addComplexIngredientDecoration
    let ingredient = skipChar '@' >>. ((attempt complexIngredient) <|> simpleIngredient)

    // Equipment
    let private addEquipmentDecoration name =
        let equipment = { Name = name }
        addEquipment equipment >>% name

    let private simpleEquipment  = manyCharsExceptWordDelimiters >>= addEquipmentDecoration
    let private complexEquipment = manyCharsTill anyButDecorationDelimiters (pstring "{}") >>= addEquipmentDecoration
    let equipment = skipChar '#' >>. ((attempt complexEquipment) <|> simpleEquipment)

    // Timer
    let private addTimerDecoration (duration, unit) =
        let timer = { Duration = duration; Unit = unit}
        addTimer timer >>% (duration.ToString() + " " + unit)

    let timer = skipString "~{" >>. (pfloat .>>. (skipChar '%' >>. (manyCharsExcept '}'))) .>> skipChar '}' >>= addTimerDecoration

    // Steps
    let convertStateToStep (directions, state) =
        Step {
            Directions = directions |> trim
            Timers = state.ParsedTimers |> List.rev
            Ingredients = state.ParsedIngredients |> List.rev
            NeededEquipment = state.ParsedEquipment |> List.rev
            Comment = state.ParsedComment |> Option.defaultValue "" 
        }

    let parseStepUntilDecoration = many1CharsExceptThese ['#'; '@'; '~' ; '/']
    let parseDecorationChar = anyOf "#@~/" |>> string
    let parseDecoration = choice [ ingredient; equipment ; timer ; inlineComment ; parseDecorationChar ]

    let stepDirections = (many (parseDecoration <|> parseStepUntilDecoration)) |>> String.concat ""
    let step = (stepDirections .>>. getUserState) |>> convertStateToStep >>= clearUserState
    
    // Line parser
    let line = choice [ commentLine ; metadata ; step ]