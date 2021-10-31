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
    // These parsers will purposefully fail to parse incomplete braces
    // thus making testing these outside the scope of these tests.
    let removeCurlyBraces s = Regex.Replace(s, @"({|})+", "");
    
    module Equipment =
        let internal testEquipmentParser stringToParse expectedValue expectedState =
            testParserWithState equipment stringToParse expectedValue [] expectedState

        [<Property(Arbitrary = [|typeof<Generators.Default>|])>]
        let ``Parses single word pieces of equipment delimited by space`` (equipmentName: SingleWordNonWhiteSpaceString) =
            let actualEquipmentName = removeCurlyBraces equipmentName.Get
            let stringToParse = @"#" + actualEquipmentName + " "
            let expectedValue = actualEquipmentName
            let parsedEquipment = ParsedEquipment { Name = actualEquipmentName }
            let expectedUserState = [ parsedEquipment ]
            testEquipmentParser stringToParse expectedValue expectedUserState

        [<Property(Arbitrary = [|typeof<Generators.Default>|])>]
        let ``Parses multi-word pieces of equipment`` (equipmentName: SingleLineNonWhiteSpaceString) =
            let actualEquipmentName = removeCurlyBraces equipmentName.Get
            let stringToParse = @"#" + actualEquipmentName + "{}"
            let expectedValue = actualEquipmentName
            let parsedEquipment = ParsedEquipment { Name = actualEquipmentName }
            let expectedUserState = [ parsedEquipment ]
            testEquipmentParser stringToParse expectedValue expectedUserState

    module Ingredient =
        let internal testIngredientParser stringToParse expectedValue expectedState =
            testParserWithState ingredient stringToParse expectedValue [] expectedState

        let formatIngredients ingredientName amountTuple =
            @"@" + ingredientName + "{" + 
                match amountTuple with
                | None -> "}"
                | Some (quantity: float, unit) -> 
                    quantity.ToString() + 
                        match unit with
                        | None -> "}"
                        | Some actualUnit -> "%" + actualUnit + "}"

        [<Property(Arbitrary = [|typeof<Generators.Default>|])>]
        let ``Parses single word ingredients delimited by space`` (ingredientName: SingleWordNonWhiteSpaceString) =
            let actualIngredientName = removeCurlyBraces ingredientName.Get
            let stringToParse = @"@" + actualIngredientName + " "
            let expectedValue = actualIngredientName
            let parsedIngredient = ParsedIngredient { Name = actualIngredientName; Amount = None }
            let expectedUserState = [ parsedIngredient ]
            testIngredientParser stringToParse expectedValue expectedUserState

        [<Property(Arbitrary = [|typeof<Generators.Default>|])>]
        let ``Parses multi-word ingredients without specified amounts`` (ingredientName: SingleLineNonWhiteSpaceString) =
            let actualIngredientName = removeCurlyBraces ingredientName.Get
            let stringToParse = formatIngredients actualIngredientName None
            let expectedValue = actualIngredientName
            let parsedIngredient = ParsedIngredient { Name = actualIngredientName; Amount = None }
            let expectedUserState = [ parsedIngredient ]
            testIngredientParser stringToParse expectedValue expectedUserState
            
        [<Property(Arbitrary = [|typeof<Generators.Default>|])>]
        let ``Parses single word ingredients with float amounts and no units`` (ingredientName: SingleWordNonWhiteSpaceString) (quantity: NormalPositiveFloat) =
            let actualIngredientName = removeCurlyBraces ingredientName.Get
            let stringToParse = formatIngredients actualIngredientName (Some (quantity.Get, None))
            let expectedValue = actualIngredientName
            let amount = { Quantity = quantity.Get; Unit = None }
            let parsedIngredient = ParsedIngredient { Name = actualIngredientName; Amount = Some amount }
            let expectedUserState = [ parsedIngredient ]
            testIngredientParser stringToParse expectedValue expectedUserState

        [<Property(Arbitrary = [|typeof<Generators.Default>|])>]
        let ``Parses multi-word ingredients with float amounts and no units`` (ingredientName: SingleLineNonWhiteSpaceString) (quantity: NormalPositiveFloat) =
            let actualIngredientName = removeCurlyBraces ingredientName.Get
            let stringToParse = formatIngredients actualIngredientName (Some (quantity.Get, None))
            let expectedValue = actualIngredientName
            let amount = { Quantity = quantity.Get; Unit = None }
            let parsedIngredient = ParsedIngredient { Name = actualIngredientName; Amount = Some amount }
            let expectedUserState = [ parsedIngredient ]
            testIngredientParser stringToParse expectedValue expectedUserState

        [<Property(Arbitrary = [|typeof<Generators.Default>|])>]
        let ``Parses single word ingredients with integer amounts and no units`` (ingredientName: SingleWordNonWhiteSpaceString) (quantity: PositiveInt) =
            let actualIngredientName = removeCurlyBraces ingredientName.Get
            let stringToParse = formatIngredients actualIngredientName (Some (float quantity.Get, None))
            let expectedValue = actualIngredientName
            let amount = { Quantity = float quantity.Get; Unit = None }
            let parsedIngredient = ParsedIngredient { Name = actualIngredientName; Amount = Some amount }
            let expectedUserState = [ parsedIngredient ]
            testIngredientParser stringToParse expectedValue expectedUserState

        [<Property(Arbitrary = [|typeof<Generators.Default>|])>]
        let ``Parses multi-word ingredients with integer amounts and no units`` (ingredientName: SingleLineNonWhiteSpaceString) (quantity: PositiveInt) =
            let actualIngredientName = removeCurlyBraces ingredientName.Get
            let stringToParse = formatIngredients actualIngredientName (Some (float quantity.Get, None))
            let expectedValue = actualIngredientName
            let amount = { Quantity = float quantity.Get; Unit = None }
            let parsedIngredient = ParsedIngredient { Name = actualIngredientName; Amount = Some amount }
            let expectedUserState = [ parsedIngredient ]
            testIngredientParser stringToParse expectedValue expectedUserState
            
        [<Property(Arbitrary = [|typeof<Generators.Default>|])>]
        let ``Parses single word ingredients with float amounts and arbitrary units`` (ingredientName: SingleWordNonWhiteSpaceString) (quantity: NormalPositiveFloat) (unit: SingleLineNonWhiteSpaceString) =
            let actualIngredientName = removeCurlyBraces ingredientName.Get
            let actualUnit = removeCurlyBraces unit.Get
            let stringToParse = formatIngredients actualIngredientName (Some (quantity.Get, Some actualUnit))
            let expectedValue = actualIngredientName
            let amount = { Quantity = quantity.Get; Unit = Some actualUnit }
            let parsedIngredient = ParsedIngredient { Name = actualIngredientName; Amount = Some amount }
            let expectedUserState = [ parsedIngredient ]
            testIngredientParser stringToParse expectedValue expectedUserState

        [<Property(Arbitrary = [|typeof<Generators.Default>|])>]
        let ``Parses multi-word ingredients with float amounts and arbitrary units`` (ingredientName: SingleLineNonWhiteSpaceString) (quantity: NormalPositiveFloat) (unit: SingleLineNonWhiteSpaceString) =
            let actualIngredientName = removeCurlyBraces ingredientName.Get
            let actualUnit = removeCurlyBraces unit.Get
            let stringToParse = formatIngredients actualIngredientName (Some (quantity.Get, Some actualUnit))
            let expectedValue = actualIngredientName
            let amount = { Quantity = quantity.Get; Unit = Some actualUnit }
            let parsedIngredient = ParsedIngredient { Name = actualIngredientName; Amount = Some amount }
            let expectedUserState = [ parsedIngredient ]
            testIngredientParser stringToParse expectedValue expectedUserState

        [<Property(Arbitrary = [|typeof<Generators.Default>|])>]
        let ``Parses single word ingredients with integer amounts and arbitrary units`` (ingredientName: SingleWordNonWhiteSpaceString) (quantity: PositiveInt) (unit: SingleLineNonWhiteSpaceString) =
            let actualIngredientName = removeCurlyBraces ingredientName.Get
            let actualUnit = removeCurlyBraces unit.Get
            let stringToParse = formatIngredients actualIngredientName (Some (float quantity.Get, Some actualUnit))
            let expectedValue = actualIngredientName
            let amount = { Quantity = float quantity.Get; Unit = Some actualUnit }
            let parsedIngredient = ParsedIngredient { Name = actualIngredientName; Amount = Some amount }
            let expectedUserState = [ parsedIngredient ]
            testIngredientParser stringToParse expectedValue expectedUserState

        [<Property(Arbitrary = [|typeof<Generators.Default>|])>]
        let ``Parses multi-word ingredients with integer amounts and arbitrary units`` (ingredientName: SingleLineNonWhiteSpaceString) (quantity: PositiveInt) (unit: SingleLineNonWhiteSpaceString) =
            let actualIngredientName = removeCurlyBraces ingredientName.Get
            let actualUnit = removeCurlyBraces unit.Get
            let stringToParse = formatIngredients actualIngredientName (Some (float quantity.Get, Some actualUnit))
            let expectedValue = actualIngredientName
            let amount = { Quantity = float quantity.Get; Unit = Some actualUnit }
            let parsedIngredient = ParsedIngredient { Name = actualIngredientName; Amount = Some amount }
            let expectedUserState = [ parsedIngredient ]
            testIngredientParser stringToParse expectedValue expectedUserState
        
    module Timer =
        let internal testTimerParser stringToParse expectedValue expectedState =
            testParserWithState timer stringToParse expectedValue [] expectedState
            
        [<Property(Arbitrary = [|typeof<Generators.Default>|])>]
        let ``Parses timers with integer amounts and arbitrary units`` (duration: PositiveInt) (unit: SingleWordNonWhiteSpaceString) =
            let actualUnit = removeCurlyBraces unit.Get
            let stringToParse = "~{" + duration.Get.ToString() + "%" + actualUnit + "}"
            let expectedValue = duration.Get.ToString() + " " + actualUnit
            let parsedIngredient = ParsedTimer { Duration = float duration.Get ; Unit = actualUnit }
            let expectedUserState = [ parsedIngredient ]
            testTimerParser stringToParse expectedValue expectedUserState
        
        [<Property(Arbitrary = [|typeof<Generators.Default>|])>]
        let ``Parses timers with float durations and arbitrary units`` (duration: NormalPositiveFloat) (unit: SingleWordNonWhiteSpaceString) =
            let actualUnit = removeCurlyBraces unit.Get
            let stringToParse = "~{" + duration.Get.ToString() + "%" + actualUnit + "}"
            let expectedValue = duration.Get.ToString() + " " + actualUnit
            let parsedIngredient = ParsedTimer { Duration = duration.Get ; Unit = actualUnit }
            let expectedUserState = [ parsedIngredient ]
            testTimerParser stringToParse expectedValue expectedUserState
