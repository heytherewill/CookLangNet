module Tests

open CookLangNet.Parser
open FParsec
open FsCheck.Xunit
open FsUnit.Xunit
open Generators
open System

let internal testParser (parser: Parser<'a, unit>) stringToParse (expectedValue: 'a) =
    let parserResult = runParserOnString parser () "Test" stringToParse 
    match parserResult with
    | Success(actualValue, _, _) -> (actualValue |> should equal expectedValue)
    | Failure _ -> failwith "Parser failed"

let internal testParserWithState (parser: Parser<'a, 'b>) stringToParse (expectedValue: 'a) initialState (expectedState: 'b) =
    let parserResult = runParserOnString parser initialState "Test" stringToParse 
    match parserResult with
    | Success(actualValue, actualState, _) -> 
        actualValue |> should equal expectedValue
        actualState |> should equal expectedState
    | Failure _ -> failwith "Parser failed"


module CommentParser =

    let internal testCommentParser stringToParse expectedValue = 
        testParser commentLine stringToParse expectedValue

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

    let internal testMetadataParser stringToParse expectedValue = 
        testParser metadata stringToParse expectedValue
        
    [<Property(Arbitrary = [|typeof<Generators.Default>|])>]
    let ``The metadata line parser reads an entire line as a metadata tuple`` (key: TrimmedSingleLineNonWhiteSpaceString) (value: SingleLineNonWhiteSpaceString) =
        let actualKey = key.Get.Replace(':', Char.MinValue)
        let metadata = actualKey + ":" + value.Get
        let stringToParse = @">> " + metadata
        let expectedValue = ParsedLine.Metadata (actualKey, value.Get.Trim())
        testMetadataParser stringToParse expectedValue

    [<Property(Arbitrary = [|typeof<Generators.Default>|])>]
    let ``The space after the comment delimiter is optional`` (key: TrimmedSingleLineNonWhiteSpaceString) (value: SingleLineNonWhiteSpaceString) =
        let actualKey = key.Get.Replace(':', Char.MinValue)
        let metadata = actualKey + ":" + value.Get
        let stringToParse = @">>" + metadata
        let expectedValue = ParsedLine.Metadata (actualKey, value.Get.Trim())
        testMetadataParser stringToParse expectedValue

module StepParser =
    module Equipment =

        let internal testEquipmentParser stringToParse expectedValue expectedState =
            testParserWithState equipment stringToParse expectedValue [] expectedState

        [<Property(Arbitrary = [|typeof<Generators.Default>|])>]
        let ``Parses single word pieces of equipment delimited by space`` (equipmentName: SingleWordNonWhiteSpaceString) =
            let stringToParse = @"#" + equipmentName.Get + " "
            let expectedValue = equipmentName.Get
            let parsedEquipment = ParsedEquipment { name = equipmentName.Get }
            let expectedUserState = [ parsedEquipment ]
            testEquipmentParser stringToParse expectedValue expectedUserState

        [<Property(Arbitrary = [|typeof<Generators.Default>|])>]
        let ``Parses multi-word pieces of equipment`` (equipmentName: SingleLineNonWhiteSpaceString) =
            let stringToParse = @"#" + equipmentName.Get + "{}"
            let expectedValue = equipmentName.Get
            let parsedEquipment = ParsedEquipment { name = equipmentName.Get }
            let expectedUserState = [ parsedEquipment ]
            testEquipmentParser stringToParse expectedValue expectedUserState

    module Ingredient =

        let internal testEquipmentParser stringToParse expectedValue expectedState =
            testParserWithState equipment stringToParse expectedValue [] expectedState

