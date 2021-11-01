module EdgeCaseTests

open CookLangNet
open CookLangNet.Parser
open Xunit

module Ingredients =
    [<Fact>]
    let ``A single-word ingredient followed by a multi-word ingredient is parsed correctly`` () = 
        let stringToParse = "These are some sample directions with added @salt and @black pepper{}."
        let expectedStep = Step {
            Directions = "These are some sample directions with added salt and black pepper."
            NeededEquipment = []
            Ingredients = [ { Name = "salt" ; Amount = None } ; { Name = "black pepper" ; Amount = None } ]
            Timers = []
            Comment = ""
        }

        ParserTests.testParser Parser.step stringToParse expectedStep

        
    [<Fact>]
    let ``A single-word equipment followed by a multi-word equipment is parsed correctly`` () = 
        let stringToParse = "These are some sample directions that require a #spatula and a #12-inch skillet{}."
        let expectedStep = Step {
            Directions = "These are some sample directions that require a spatula and a 12-inch skillet."
            NeededEquipment = [ { Name = "spatula" } ; { Name = "12-inch skillet" } ]
            Ingredients = []
            Timers = []
            Comment = ""
        }

        ParserTests.testParser Parser.step stringToParse expectedStep