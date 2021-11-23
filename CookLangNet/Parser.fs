namespace CookLangNet

open FParsec
    
module internal Parser =
    (*==================================*)
    (* Intermediate state parser models *)
    (*==================================*)

    /// User state that holds the decorations parsed in each step.
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

    /// Holds the data parsed for each line.
    type ParsedLine =
        | Comment of string
        | Metadata of string * string
        | Step of Step

    (*==================================*)
    (*          Helper parsers          *)
    (*==================================*)

    /// Parses at least 1 of any character except for the ones contained in `chars`.
    let private many1CharsExceptThese chars = many1Chars (noneOf chars)
    /// Parses any character except `char`.
    let private manyCharsExcept char = manyChars (noneOf [ char ])
    /// Parses any character except `char` and then parses `char`. Returns the parsed chars as a string, without `char` at the end.
    let private manyCharsEndingWith char = manyChars (noneOf [ char ]) .>> pchar char
    /// Parses any number of characters until the CookLang delimiters for a single word are parsed.
    /// The delimiters are whitespace, line breaks, eof, comma and period.
    let private manyCharsExceptWordDelimiters = manyChars (noneOf [ ',' ; '.' ; '\n'; '\r'; ' ' ])
    /// Parses any char except for @, # and ~.
    let private anyButDecorationDelimiters = noneOf "@#~"
    
    (*==================================*)
    (*      User State manipulation     *)
    (*==================================*)
    
    /// Adds the parsed comment to the user state.
    let private addComment c = updateUserState (fun s -> { s with ParsedComment = Some c })
    /// Adds a parsed ingredient to the user state.
    let private addIngredient i = updateUserState (fun s -> { s with ParsedIngredients = i::s.ParsedIngredients })
    /// Adds a parsed equipment to the user state.
    let private addEquipment e = updateUserState (fun s -> { s with ParsedEquipment = e::s.ParsedEquipment })
    /// Adds a parsed timer to the user state.
    let private addTimer t = updateUserState (fun s -> { s with ParsedTimers = t::s.ParsedTimers })
    /// Resets the user state. This is meant to be called when one finishes parsing a line.
    let private clearUserState s = updateUserState (fun _ -> State.Empty) >>% s

    (*==================================*)
    (*             Comments             *)
    (*==================================*)

    /// Parses the comment delimiter `--` and then the rest of the line. The output is trimmed.
    let private comment = skipString "--" >>. restOfLine true |>> trim
    /// Parses an entire line as a comment.
    let commentLine = comment |>> ParsedLine.Comment
    /// Parses the rest of a line as a comment.
    let inlineComment = comment >>= addComment >>% ""
    
    (*==================================*)
    (*             Metadata             *)
    (*==================================*)

    /// Constructs a metadata object from a list of strings.
    let private metadataFromStringParts (list: string list) =
        let key = list.Head |> trim
        let value = list.Tail |> String.concat ":" |> trim
        Metadata(key, value)

    /// Parses an entire line as a metadata entry.
    let metadata =
        skipString ">>"
        >>. sepBy1 (manyCharsExcept ':') (pchar ':') 
        |>> metadataFromStringParts
        
    (*==================================*)
    (*           Ingredients            *)
    (*==================================*)

    /// Constructs an IngredientAmount object from a tuple.
    let private toIngredientAmount (quantity, unit) = 
        { Quantity = quantity; Unit = unit }

    /// Adds an ingredient to the user state and returns its name as the parser's result.
    let private addComplexIngredientDecoration (name, amount) =
        let ingredient = { Name = name; Amount = amount }
        addIngredient ingredient >>% name
    
    /// Adds an ingredient to the user state and returns its name as the parser's result.
    let private addSimpleIngredientDecoration name =
        addComplexIngredientDecoration (name, None)

    /// Parses a single-word ingredient with no amounts.
    let private simpleIngredient        = manyCharsExceptWordDelimiters >>= addSimpleIngredientDecoration
    /// Parses the name of a multi-word ingredient or of an ingredient that contains amounts.
    let private complexIngredientName   = manyCharsTill anyButDecorationDelimiters (pchar '{')
    /// Parses the amount for an ingredient.
    let private complexIngredientAmount = pfloat .>>. (opt (skipChar '%' >>. (manyCharsExcept '}'))) |>> toIngredientAmount
    /// Parses a multi-word ingredient or an ingredient that contains amounts.    
    let private complexIngredient = (complexIngredientName .>>. (opt complexIngredientAmount) .>> skipChar '}') >>= addComplexIngredientDecoration
    /// Parses an ingredient's name and adds the ingredient object to the user state.
    let ingredient = skipChar '@' >>. ((attempt complexIngredient) <|> simpleIngredient)
    
    (*==================================*)
    (*            Equipment             *)
    (*==================================*)

    /// Adds an equipment to the user state and returns its name as the parser's result.
    let private addEquipmentDecoration name =
        let equipment = { Name = name }
        addEquipment equipment >>% name

    /// Parses a single-word equipment.
    let private simpleEquipment  = manyCharsExceptWordDelimiters >>= addEquipmentDecoration
    /// Parses a multi-word equipment.
    let private complexEquipment = manyCharsTill anyButDecorationDelimiters (pstring "{}") >>= addEquipmentDecoration
    /// Parses an equipment's name and adds the equipment object to the user state.
    let equipment = skipChar '#' >>. ((attempt complexEquipment) <|> simpleEquipment)
    
    (*==================================*)
    (*              Timer               *)
    (*==================================*)
    
    /// Adds a timer to the user state and returns its duration and unit as the parser's result.
    let private addTimerDecoration (name, (duration, unit)) =
        let timer = { Name = name; Duration = duration; Unit = unit }
        addTimer timer >>% (duration.ToString() + " " + unit)

    /// Parses a timer and adds the timer object to the user state.
    let timer = skipChar '~' >>. manyCharsTill anyButDecorationDelimiters (pchar '{') .>>. (pfloat .>>. (skipChar '%' >>. (manyCharsEndingWith '}'))) >>= addTimerDecoration

    (*==================================*)
    (*              Steps               *)
    (*==================================*)

    /// Constructs a step object from a tuple.
    let private convertStateToStep (directions, state) =
        Step {
            Directions = directions |> trim
            Timers = state.ParsedTimers |> List.rev
            Ingredients = state.ParsedIngredients |> List.rev
            Equipment = state.ParsedEquipment |> List.rev
            Comment = state.ParsedComment |> Option.defaultValue "" 
        }

    /// Parses many characters as part of the step's description until one of the decoration delimiters is found.
    let private parseStepUntilDecoration = many1CharsExceptThese ['#'; '@'; '~' ; '-']
    /// Parses a single decoration char as a string.
    /// This is a fallback for when a decoration char is just part of the description and not part of a decoration.
    let private parseDecorationChar = anyOf "#@~-" |>> string
    /// Parses any of the special elements in a description: ingredients, equipments, timers and comments.
    /// If the parser fails to parse any of those it will consume the first character and return it.
    let private parseDecoration = choice [ ingredient; equipment ; timer ; inlineComment ; parseDecorationChar ]
    /// Parses the directions of the step by applying `parseDecoration` and `parseStepUntilDecoration` many times and concatenating the results.
    let private stepDirections = (many (parseDecoration <|> parseStepUntilDecoration)) |>> String.concat ""
    /// Parses a line as a step of the recipe.
    let step = (stepDirections .>>. getUserState) |>> convertStateToStep >>= clearUserState
    
    /// Parses a line of the recipe, which can be either a comment, a metadata entry or a step.
    let line = choice [ commentLine ; metadata ; step ]