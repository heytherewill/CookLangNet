module ParserTests

(* These test the individual parsers to ensure they behave as expected. *)

open CookLangNet.Parser
open FParsec
open FsCheck.Xunit
open FsUnit.Xunit
open Generators
open System

/// Compares the outcome of a parser with an expected value.
let internal testParser parser stringToParse (expectedValue: 'Value) =
    let parserResult = runParserOnString parser State.Empty "Test" stringToParse

    match parserResult with
    | Success(actualValue, _, _) -> (actualValue |> should equal expectedValue)
    | Failure(f, _, _) -> failwith (sprintf "Fail to parse string %s" f)

/// Compares the outcome of a parser with an expected value.
let internal testParserWithState
    (parser: Parser<'Value, State>)
    stringToParse
    (expectedValue: 'Value)
    (expectedState: State)
    =
    let parserResult = runParserOnString parser State.Empty "Test" stringToParse

    match parserResult with
    | Success(actualValue, actualState, _) ->
        actualValue |> should equal expectedValue
        actualState |> should equal expectedState
    | Failure(f, _, _) -> failwith (sprintf "Fail to parse string %s" f)

module CommentParser =
    let internal testCommentParser stringToParse expectedValue =
        testParser commentLine stringToParse expectedValue

    [<Property(Arbitrary = [| typeof<Generators.Default> |])>]
    let ``The comment line parser reads an entire line as a comment`` (input: SingleLineString) =
        let comment = input.Get
        let stringToParse = @"-- " + comment
        let expectedValue = ParsedLine.Comment(comment.Trim())
        testCommentParser stringToParse expectedValue

    [<Property(Arbitrary = [| typeof<Generators.Default> |])>]
    let ``The space after the comment delimiter is optional`` (input: SingleLineString) =
        let comment = input.Get
        let stringToParse = @"--" + comment
        let expectedValue = ParsedLine.Comment(comment.Trim())
        testCommentParser stringToParse expectedValue

module MetadataParser =
    let internal testMetadataParser stringToParse expectedValue =
        testParser metadata stringToParse expectedValue

    [<Property(Arbitrary = [| typeof<Generators.Default> |])>]
    let ``The metadata line parser reads an entire line as a metadata tuple``
        (key: TrimmedSingleLineString)
        (value: SingleLineString)
        =
        let actualKey = key.Get.Replace(':', Char.MinValue)
        let metadata = actualKey + ":" + value.Get
        let stringToParse = @">> " + metadata
        let expectedValue = ParsedLine.Metadata(actualKey, value.Get.Trim())
        testMetadataParser stringToParse expectedValue

    [<Property(Arbitrary = [| typeof<Generators.Default> |])>]
    let ``The space after the comment delimiter is optional`` (key: TrimmedSingleLineString) (value: SingleLineString) =
        let actualKey = key.Get.Replace(':', Char.MinValue)
        let metadata = actualKey + ":" + value.Get
        let stringToParse = @">>" + metadata
        let expectedValue = ParsedLine.Metadata(actualKey, value.Get.Trim())
        testMetadataParser stringToParse expectedValue

module StepParser =

    module Cookware =
        let internal testCookwareParser stringToParse expectedValue expectedState =
            testParserWithState cookware stringToParse expectedValue expectedState

        [<Property(Arbitrary = [| typeof<Generators.Default> |])>]
        let ``Parses single word pieces of cookware delimited by space`` (cookware: SingleWordCookware) =
            let stringToParse = cookware.Serialize() + " "

            let expectedUserState =
                { State.Empty with
                    ParsedCookware = [ cookware.Get ] }

            testCookwareParser stringToParse (cookware.ToString()) expectedUserState

        [<Property(Arbitrary = [| typeof<Generators.Default> |])>]
        let ``Parses single word pieces of cookware delimited by a comma`` (cookware: SingleWordCookware) =
            let stringToParse = cookware.Serialize() + ","

            let expectedUserState =
                { State.Empty with
                    ParsedCookware = [ cookware.Get ] }

            testCookwareParser stringToParse (cookware.ToString()) expectedUserState

        [<Property(Arbitrary = [| typeof<Generators.Default> |])>]
        let ``Parses single word pieces of cookware delimited by a period`` (cookware: SingleWordCookware) =
            let stringToParse = cookware.Serialize() + "."

            let expectedUserState =
                { State.Empty with
                    ParsedCookware = [ cookware.Get ] }

            testCookwareParser stringToParse (cookware.ToString()) expectedUserState

        [<Property(Arbitrary = [| typeof<Generators.Default> |])>]
        let ``Parses multi-word pieces of cookware`` (cookware: MultiWordCookware) =
            let stringToParse = cookware.Serialize()

            let expectedUserState =
                { State.Empty with
                    ParsedCookware = [ cookware.Get ] }

            testCookwareParser stringToParse (cookware.ToString()) expectedUserState

    module Ingredient =
        let internal testIngredientParser stringToParse expectedValue expectedState =
            testParserWithState ingredient stringToParse expectedValue expectedState

        [<Property(Arbitrary = [| typeof<Generators.Default> |])>]
        let ``Parses single word ingredients delimited by space`` (ingredient: SingleWordNoAmountIngredient) =
            let stringToParse = ingredient.Serialize() + " "
            let expectedValue = ingredient.ToString()

            let expectedUserState =
                { State.Empty with
                    ParsedIngredients = [ ingredient.Get ] }

            testIngredientParser stringToParse expectedValue expectedUserState

        [<Property(Arbitrary = [| typeof<Generators.Default> |])>]
        let ``Parses single word ingredients delimited by a comma`` (ingredient: SingleWordNoAmountIngredient) =
            let stringToParse = ingredient.Serialize() + ","
            let expectedValue = ingredient.ToString()

            let expectedUserState =
                { State.Empty with
                    ParsedIngredients = [ ingredient.Get ] }

            testIngredientParser stringToParse expectedValue expectedUserState

        [<Property(Arbitrary = [| typeof<Generators.Default> |])>]
        let ``Parses single word ingredients delimited by a period`` (ingredient: SingleWordNoAmountIngredient) =
            let stringToParse = ingredient.Serialize() + "."
            let expectedValue = ingredient.ToString()

            let expectedUserState =
                { State.Empty with
                    ParsedIngredients = [ ingredient.Get ] }

            testIngredientParser stringToParse expectedValue expectedUserState

        [<Property(Arbitrary = [| typeof<Generators.Default> |])>]
        let ``Parses multi-word ingredients without specified amounts`` (ingredient: MultiWordNoAmountIngredient) =
            let stringToParse = ingredient.Serialize()
            let expectedValue = ingredient.ToString()

            let expectedUserState =
                { State.Empty with
                    ParsedIngredients = [ ingredient.Get ] }

            testIngredientParser stringToParse expectedValue expectedUserState

        [<Property(Arbitrary = [| typeof<Generators.Default> |])>]
        let ``Parses single word ingredients with with arbitrary amounts``
            (ingredient: SingleWordWithAmountIngredient)
            =
            let stringToParse = ingredient.Serialize()
            let expectedValue = ingredient.ToString()

            let expectedUserState =
                { State.Empty with
                    ParsedIngredients = [ ingredient.Get ] }

            testIngredientParser stringToParse expectedValue expectedUserState

        [<Property(Arbitrary = [| typeof<Generators.Default> |])>]
        let ``Parses multi word ingredients with with arbitrary amounts`` (ingredient: MultiWordWithAmountIngredient) =
            let stringToParse = ingredient.Serialize()
            let expectedValue = ingredient.ToString()

            let expectedUserState =
                { State.Empty with
                    ParsedIngredients = [ ingredient.Get ] }

            testIngredientParser stringToParse expectedValue expectedUserState

    module Timer =
        let internal testTimerParser stringToParse expectedValue expectedState =
            testParserWithState timer stringToParse expectedValue expectedState

        [<Property(Arbitrary = [| typeof<Generators.Default> |])>]
        let ``Parses timers with arbitrary amounts and units`` (timer: ValidTimer) =
            let stringToParse = timer.Serialize()
            let expectedValue = timer.ToString()

            let expectedUserState =
                { State.Empty with
                    ParsedTimers = [ timer.Get ] }

            testTimerParser stringToParse expectedValue expectedUserState
