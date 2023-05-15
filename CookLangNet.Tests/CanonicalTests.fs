/// This file runs the canonical list of tests contained in the
/// official cooklang spec repository. The repo is included as
/// a git submodule and the yaml file containing the canonical
/// testsuite is parsed here.
module CanonicalTests

open CookLangNet
open System.IO
open System.Collections.Generic
open Xunit
open YamlDotNet.Serialization
open YamlDotNet.Serialization.NamingConventions
open System

type CanonicalTestIngredient = { Name: string; quantity: int; units: string }

type Step = 
| Ingredient of CanonicalTestIngredient
| Text of string

// NOTE: These can't be made into records because YamlDotNet 
// requires an empty constructor.
type YamlCanonicalTest() =
    member val Source: string = "" with get, set
    member val Result: IDictionary<string, obj> = null with get, set

type YamlCanonicalTestFile() =
    member val Version: int = 0 with get, set
    member val Tests: IDictionary<string, YamlCanonicalTest> = null with get, set

type CanonicalTest = {
    Name: string;
    Source: string;
    ExpectedRecipe: Recipe;
    TestSuiteVersion: int
}

module TestFailedDueTo = 
    let parsing canonicalTest errorMessage =

        let errorMessage = 
            sprintf """
    Test Suite version %d
    Parsing the source for test %s threw the exception:

    %s""" 
                canonicalTest.TestSuiteVersion canonicalTest.Name errorMessage

        failwith errorMessage

let canonicalTestData () =
    let deserializer = 
        DeserializerBuilder()
            .WithNamingConvention(CamelCaseNamingConvention.Instance)
            .Build()

    let file = File.OpenText("../../../../spec/tests/canonical.yaml")
    let canonicalTestFile = deserializer.Deserialize<YamlCanonicalTestFile>(file)

    let recipeFromYaml (yamlDictionary: IDictionary<string, obj>) =
    
        let steps =
            let toStep (rawItem: obj) =
                let mutable directions = ""
                let mutable ingredients = []
                let mutable cookware = []
                let mutable timers = []

                let itemList = rawItem :?> IEnumerable<obj>
                for item in itemList do
                    let itemDictionary = item :?> IDictionary<obj, obj>

                    match itemDictionary["type"].ToString() with
                    | "text" -> 
                        directions <- directions + (itemDictionary["value"].ToString())
                    | "ingredient" ->
                        let name = itemDictionary["name"].ToString()
                        let quantity = 
                            let quantityString = itemDictionary["quantity"].ToString()
                            if quantityString = "some" then None else quantityFromString quantityString |> Some
                        let units = itemDictionary["units"].ToString()
                        let parsedIngredient = {
                            Name = name
                            Amount = 
                                match quantity with
                                | None -> None
                                | Some quantity -> Some {
                                    Quantity = quantity ;
                                    Unit = 
                                        if units = "" then None
                                        else Some units
                                }
                        }
                        directions <- directions + name
                        ingredients <- parsedIngredient::ingredients
                    | "timer" -> 
                        let name = itemDictionary["name"].ToString()
                        let unit = itemDictionary["units"].ToString()
                        let duration = float (itemDictionary["quantity"].ToString())
                        let parsedTimer = {
                            Name = name
                            Unit = unit
                            Duration = duration
                        }
                        directions <- directions + (duration.ToString()) + " " + unit 
                        timers <- parsedTimer::timers
                    | "cookware" -> 
                        let name = itemDictionary["name"].ToString()
                        let quantity = itemDictionary["quantity"].ToString()
                        let parsedCookware = {
                            Name = itemDictionary["name"].ToString()
                            Quantity = quantityFromString quantity
                        }
                        directions <- directions + name
                        cookware <- parsedCookware::cookware
                    | _ -> failwith "Invalid type of step."

                {
                    Directions = directions.Trim()
                    Ingredients = ingredients |> List.rev
                    Cookware = cookware |> List.rev
                    Timers = timers |> List.rev
                }

            yamlDictionary["steps"] :?> List<obj>
            |> Seq.map toStep
            |> List.ofSeq

        let metadata = 
            yamlDictionary["metadata"] :?> IDictionary<obj, obj>
            |> Seq.map (fun kvPair -> kvPair.Key.ToString(), kvPair.Value.ToString())
            |> dict

        {
            Steps = steps ;
            Metadata = metadata
        }

    seq {
        for test in canonicalTestFile.Tests ->
            [
                { 
                    Name = test.Key;
                    Source = test.Value.Source;
                    TestSuiteVersion = canonicalTestFile.Version;
                    ExpectedRecipe = recipeFromYaml test.Value.Result
                } :> obj
            ] |> Seq.toArray
    }

[<Theory>]
[<MemberData("canonicalTestData")>]
let ``The canonical suite of tests passes`` canonicalTest =
    match CookLangParser.ParseString canonicalTest.Source with
    | Failure errorMessage -> TestFailedDueTo.parsing canonicalTest errorMessage        
    | Success recipe -> recipe |> shouldBeTheSameRecipeAs canonicalTest.ExpectedRecipe