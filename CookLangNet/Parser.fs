namespace CookLangNet

open FParsec
    
module internal Parser =
    // Intermediate state parser models
    type ParsedStepDecorations = 
        | ParsedEquipment of Equipment

    type ParsedLine =
        | Comment of string
        | Metadata of string * string

    // Helper parsers
    let manyCharsExcept char = manyChars (noneOf [ char ])
    let anyCharsTillParser p = manyCharsTill anyChar p
    let anyCharsTillChar c = anyCharsTillParser (pchar c)
    let anyCharsTillString s = anyCharsTillParser (pstring s)
    let anyCharsTillSpace = anyCharsTillParser spaces1 

    // Helper functions
    let trim (s: string) = s.Trim()
        
    // Type aliases
    type CookLangParser<'a> = Parser<'a, unit>
    type StepParser = Parser<string, ParsedStepDecorations list>

    // User State manipulation
    let addDecoration d = updateUserState (fun decorations -> d::decorations)

    // Comments
    let comment : CookLangParser<string> = skipString "//" >>. restOfLine true |>> trim
    let commentLine = comment |>> ParsedLine.Comment

    // Metadata
    let metadataFromStringParts (list: string list) =
        let key = list.Head.Trim()
        let value = list.Tail |> String.concat ":" |> trim
        Metadata(key, value)

    let metadata : CookLangParser<ParsedLine> = 
        skipString ">>"
        >>. sepBy1 (manyCharsExcept ':') (pchar ':') 
        |>> metadataFromStringParts

    // Equipment
    let private addEquipmentDecoration name =
        let equipment = ParsedEquipment { name = name }
        addDecoration equipment >>% name

    let private simpleEquipment  = anyCharsTillSpace       >>= addEquipmentDecoration
    let private complexEquipment = anyCharsTillString "{}" >>= addEquipmentDecoration 
    let equipment = pchar '#' >>. ((attempt complexEquipment) <|> simpleEquipment)

