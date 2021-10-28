namespace CookLangNet

open FParsec
    
module internal Parser =
    // Helper parsers
    let manyCharsExcept char =
        manyChars (noneOf [ char ])

    // Helper functions
    let trim (s: string) = s.Trim()

    // Intermediate state parser models
    type ParsedLine =
        | Comment of string
        | Metadata of string * string
        
    // Type aliases
    type CookLangParser<'a> = Parser<'a, unit>

    // Comments
    let comment     : CookLangParser<string>     = skipString "//" >>. restOfLine true |>> trim
    let commentLine : CookLangParser<ParsedLine> = comment |>> ParsedLine.Comment
    // Metadata
    let metadataFromStringParts (list: string list) =
        let key = list.Head.Trim()
        let value = list.Tail |> String.concat ":" |> trim
        Metadata(key, value)

    let metadata : CookLangParser<ParsedLine> = 
        skipString ">>"
        >>. sepBy1 (manyCharsExcept ':') (pchar ':') 
        |>> metadataFromStringParts

