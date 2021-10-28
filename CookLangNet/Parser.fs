namespace CookLangNet

open FParsec
    
module internal Parser =
    // Helper functions
    let trim (s: string) = s.Trim()

    // Intermediate state parser models
    type ParsedLine =
        | Comment of string
        
    // Type aliases
    type CookLangParser<'a> = Parser<'a, unit>

    // Comments
    let comment     : CookLangParser<string>     = skipString "//" >>. restOfLine true |>> trim
    let commentLine : CookLangParser<ParsedLine> = comment |>> ParsedLine.Comment
