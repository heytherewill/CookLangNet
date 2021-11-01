module ParserTests

open CookLangNet
open CookLangNet.Parser
open FParsec
open FsCheck.Xunit
open FsUnit.Xunit
open Generators
open System
open FsCheck
open System.Text.RegularExpressions

let internal testParser parser stringToParse (expectedValue: 'a) =
    let parserResult = runParserOnString parser [] "Test" stringToParse 
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
    let ``The comment line parser reads an entire line as a comment`` (input: SingleLineString) =
        let comment = input.Get
        let stringToParse = @"// " + comment
        let expectedValue = ParsedLine.Comment (comment.Trim())
        testCommentParser stringToParse expectedValue
        
    [<Property(Arbitrary = [|typeof<Generators.Default>|])>]
    let ``The space after the comment delimiter is optional``(input: SingleLineString) =
        let comment = input.Get
        let stringToParse = @"//" + comment
        let expectedValue = ParsedLine.Comment (comment.Trim())
        testCommentParser stringToParse expectedValue

module MetadataParser = 
    let internal testMetadataParser stringToParse expectedValue = 
        testParser metadata stringToParse expectedValue
        
    [<Property(Arbitrary = [|typeof<Generators.Default>|])>]
    let ``The metadata line parser reads an entire line as a metadata tuple`` (key: TrimmedSingleLineString) (value: SingleLineString) =
        let actualKey = key.Get.Replace(':', Char.MinValue)
        let metadata = actualKey + ":" + value.Get
        let stringToParse = @">> " + metadata
        let expectedValue = ParsedLine.Metadata (actualKey, value.Get.Trim())
        testMetadataParser stringToParse expectedValue

    [<Property(Arbitrary = [|typeof<Generators.Default>|])>]
    let ``The space after the comment delimiter is optional`` (key: TrimmedSingleLineString) (value: SingleLineString) =
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
        let ``Parses single word pieces of equipment delimited by space`` (equipment: SingleWordEquipment) =
            let stringToParse = equipment.Serialize() + " "
            let parsedEquipment = ParsedEquipment equipment.Get
            let expectedUserState = [ parsedEquipment ]
            testEquipmentParser stringToParse (equipment.ToString()) expectedUserState

        [<Property(Arbitrary = [|typeof<Generators.Default>|])>]
        let ``Parses single word pieces of equipment delimited by a comma``(equipment: SingleWordEquipment) =
                   let stringToParse = equipment.Serialize() + ","
                   let parsedEquipment = ParsedEquipment equipment.Get
                   let expectedUserState = [ parsedEquipment ]
                   testEquipmentParser stringToParse (equipment.ToString()) expectedUserState

        [<Property(Arbitrary = [|typeof<Generators.Default>|])>]
        let ``Parses single word pieces of equipment delimited by a period``(equipment: SingleWordEquipment) =
                   let stringToParse = equipment.Serialize() + "."
                   let parsedEquipment = ParsedEquipment equipment.Get
                   let expectedUserState = [ parsedEquipment ]
                   testEquipmentParser stringToParse (equipment.ToString()) expectedUserState

        [<Property(Arbitrary = [|typeof<Generators.Default>|])>]
        let ``Parses multi-word pieces of equipment`` (equipment: MultiWordEquipment) =
            let stringToParse = sprintf "#%s{}" (equipment.ToString())
            let parsedEquipment = ParsedEquipment equipment.Get
            let expectedUserState = [ parsedEquipment ]
            testEquipmentParser stringToParse (equipment.ToString()) expectedUserState

    module Ingredient =
        let internal testIngredientParser stringToParse expectedValue expectedState =
            testParserWithState ingredient stringToParse expectedValue [] expectedState

        [<Property(Arbitrary = [|typeof<Generators.Default>|])>]
        let ``Parses single word ingredients delimited by space`` (ingredient: SingleWordNoAmountIngredient) =
            let stringToParse = ingredient.Serialize() + " "
            let expectedValue = ingredient.ToString()
            let parsedIngredient = ParsedIngredient ingredient.Get
            let expectedUserState = [ parsedIngredient ]
            testIngredientParser stringToParse expectedValue expectedUserState

        [<Property(Arbitrary = [|typeof<Generators.Default>|])>]
        let ``Parses single word ingredients delimited by a comma`` (ingredient: SingleWordNoAmountIngredient) =
            let stringToParse = ingredient.Serialize() + ","
            let expectedValue = ingredient.ToString()
            let parsedIngredient = ParsedIngredient ingredient.Get
            let expectedUserState = [ parsedIngredient ]
            testIngredientParser stringToParse expectedValue expectedUserState
            

        [<Property(Arbitrary = [|typeof<Generators.Default>|])>]
        let ``Parses single word ingredients delimited by a period`` (ingredient: SingleWordNoAmountIngredient) =
            let stringToParse = ingredient.Serialize() + "."
            let expectedValue = ingredient.ToString()
            let parsedIngredient = ParsedIngredient ingredient.Get
            let expectedUserState = [ parsedIngredient ]
            testIngredientParser stringToParse expectedValue expectedUserState

        [<Property(Arbitrary = [|typeof<Generators.Default>|])>]
        let ``Parses multi-word ingredients without specified amounts`` (ingredient: MultiWordNoAmountIngredient) =
            let stringToParse = ingredient.Serialize()
            let expectedValue = ingredient.ToString()
            let parsedIngredient = ParsedIngredient ingredient.Get
            let expectedUserState = [ parsedIngredient ]
            testIngredientParser stringToParse expectedValue expectedUserState
            
        [<Property(Arbitrary = [|typeof<Generators.Default>|])>]
        let ``Parses single word ingredients with with arbitrary amounts`` (ingredient: SingleWordWithAmountIngredient)=
            let stringToParse = ingredient.Serialize()
            let expectedValue = ingredient.ToString()
            let parsedIngredient = ParsedIngredient ingredient.Get
            let expectedUserState = [ parsedIngredient ]
            testIngredientParser stringToParse expectedValue expectedUserState
            
        [<Property(Arbitrary = [|typeof<Generators.Default>|])>]
        let ``Parses multi word ingredients with with arbitrary amounts`` (ingredient: MultiWordWithAmountIngredient) =
            let stringToParse = ingredient.Serialize()
            let expectedValue = ingredient.ToString()
            let parsedIngredient = ParsedIngredient ingredient.Get
            let expectedUserState = [ parsedIngredient ]
            testIngredientParser stringToParse expectedValue expectedUserState

    module Timer =
        let internal testTimerParser stringToParse expectedValue expectedState =
            testParserWithState timer stringToParse expectedValue [] expectedState
            
        [<Property(Arbitrary = [|typeof<Generators.Default>|])>]
        let ``Parses timers with arbitrary amounts and units`` (timer: ValidTimer) =
            let stringToParse = timer.Serialize()
            let expectedValue = timer.ToString()
            let parsedTimer = ParsedTimer timer.Get
            let expectedUserState = [ parsedTimer ]
            testTimerParser stringToParse expectedValue expectedUserState
