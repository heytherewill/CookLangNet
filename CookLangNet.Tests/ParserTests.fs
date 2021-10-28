module Tests

open CookLangNet.Parser
open FParsec
open FsCheck.Xunit
open FsUnit.Xunit
open Generators
open System

let testParserSuccess parser stringToParse expectedValue =
    let parserResult = runParserOnString parser () "Test" stringToParse 
    match parserResult with
    | Success(actualValue, _, _) -> (actualValue |> should equal expectedValue)
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

module MetadataParser = 

    let testMetadataParser stringToParse expectedValue = 
        testParserSuccess metadata stringToParse expectedValue
        
    [<Property(Arbitrary = [|typeof<Generators.Default>|])>]
    let ``The metadata line parser reads an entire line as a metadata tuple`` (key: SingleLineNonWhiteSpaceString) (value: SingleLineNonWhiteSpaceString) =
        let actualKey = key.Get.Replace(':', Char.MinValue).Trim()
        let metadata = actualKey + ":" + value.Get
        let stringToParse = @">> " + metadata
        let expectedValue = ParsedLine.Metadata (actualKey, value.Get.Trim())
        testMetadataParser stringToParse expectedValue

    [<Property(Arbitrary = [|typeof<Generators.Default>|])>]
    let ``The space after the comment delimiter is optional`` (key: SingleLineNonWhiteSpaceString) (value: SingleLineNonWhiteSpaceString) =
        let actualKey = key.Get.Replace(':', Char.MinValue).Trim()
        let metadata = actualKey + ":" + value.Get
        let stringToParse = @">>" + metadata
        let expectedValue = ParsedLine.Metadata (actualKey, value.Get.Trim())
        testMetadataParser stringToParse expectedValue
