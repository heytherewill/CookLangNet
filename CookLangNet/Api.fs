namespace CookLangNet

open FParsec
open Parser
open System.IO

module CookLangParser =
    ///<summary>Parses the passed string into a Recipe.</summary>
    ///<param name="stringToParse">The input string.</param>
    ///<returns>The parsed recipe.</returns>
    ///<exception cref="CookLangNet.CSharp.CookLangParserException">Thrown when the input is an invalid Recipe.</exception>
    let parse string =
        use reader = new StringReader(string)
        let mutable parsedLines = []
        let mutable lineToParse = reader.ReadLine()
        let mutable err: string = null

        while isNotNull lineToParse do
            if lineToParse <> "" then
                match runParserOnString line State.Empty "CookLang Parser" lineToParse with
                | Success(parsedLine, _, _) -> parsedLines <- parsedLine :: parsedLines
                | Failure(errAsString, _, _) ->
                    lineToParse <- null
                    err <- errAsString

            if isNull err then
                lineToParse <- reader.ReadLine()

        if isNotNull err then
            CookLangParserResult.Failure err
        else
            let finalParsedLines = parsedLines |> List.rev

            CookLangParserResult.Success
                { Steps =
                    finalParsedLines
                    |> List.choose (function
                        | Step s -> Some s
                        | _ -> None)
                  Metadata =
                    finalParsedLines
                    |> List.choose (function
                        | Metadata(key, value) -> Some(key, value)
                        | _ -> None)
                    |> dict }
