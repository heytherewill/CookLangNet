namespace CookLangNet

open FParsec
open Parser
open System.IO

module CookLangParser =
    
    let private parseStringIntoLines s =
    
        use reader = new StringReader(s)
        let mutable parsedLines = []
        let mutable lineToParse = reader.ReadLine()

        while (not (isNull lineToParse)) do
            if (lineToParse <> "") then
                match runParserOnString line [] "CookLang Parser" lineToParse with
                | Success (parsedLine, _, _) -> parsedLines <- parsedLine::parsedLines
                | Failure (erAsString, err, state) -> 
                    printfn "%s %s %s" erAsString (err.ToString()) (state.ToString())
                    ()
            lineToParse <- reader.ReadLine()

        parsedLines |> List.rev

    let ParseString stringToParse =
        let parsedLines = parseStringIntoLines stringToParse

        let metadata = 
            parsedLines 
            |> List.choose (function Metadata (key, value) -> Some (key, value) | _ -> None)
            |> dict

        {
            Steps = parsedLines |> List.choose (function Step s -> Some s | _ -> None)
            Metadata = metadata
        }