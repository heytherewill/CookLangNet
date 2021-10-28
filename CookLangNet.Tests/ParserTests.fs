module Tests

open CookLangNet.Parser
open FParsec
open FsCheck.Xunit
open FsUnit.Xunit
open Generators

let testParserSuccess parser stringToParse expectedValue =
    match runParserOnString parser () "Test" stringToParse with
    | Success(actualValue, _, _) -> (expectedValue |> should equal actualValue)
    | Failure _ -> failwith "Parser failed"

module CommentParser =

    let testCommentParser stringToParse expectedValue = 
        testParserSuccess commentLine stringToParse expectedValue

    [<Property(Arbitrary = [|typeof<Generators.Default>|])>]
    let ``The comment line parser reads an entire line as a comment`` (input: SingleLineNonWhiteSpaceString) =
        let comment = input.Get
        let stringToParse = @"// " + comment
        let expectedValue = ParsedLine.Comment (comment.Trim())
        testCommentParser stringToParse expectedValue
        
    [<Property(Arbitrary = [|typeof<Generators.Default>|])>]
    let ``The space after the comment delimiter is optional``(input: SingleLineNonWhiteSpaceString) =
        let comment = input.Get
        let stringToParse = @"//" + comment
        let expectedValue = ParsedLine.Comment (comment.Trim())
        testCommentParser stringToParse expectedValue