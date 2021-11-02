module RegressionTests

open CookLangNet
open CookLangNet.Parser
open Xunit

module Ingredients =
    [<Fact>]
    let ``A single-word ingredient followed by a multi-word ingredient is parsed correctly`` () = 
        let stringToParse = "These are some sample directions with added @salt and @black pepper{}."
        let expectedStep = Step {
            Directions = "These are some sample directions with added salt and black pepper."
            Equipment = []
            Ingredients = [ { Name = "salt" ; Amount = None } ; { Name = "black pepper" ; Amount = None } ]
            Timers = []
            Comment = ""
        }

        ParserTests.testParser Parser.step stringToParse expectedStep
        
    [<Fact>]
    let ``A single-word ingredient followed by a multi-word equipment is parsed correctly`` () = 
        let stringToParse = "These are some sample directions with added @salt via a #salt shaker{}."
        let expectedStep = Step {
            Directions = "These are some sample directions with added salt via a salt shaker."
            Equipment = [ { Name = "salt shaker" } ]
            Ingredients = [ { Name = "salt" ; Amount = None } ]
            Timers = []
            Comment = ""
        }

        ParserTests.testParser Parser.step stringToParse expectedStep
        
    [<Fact>]
    let ``A single-word ingredient followed by a timer is parsed correctly`` () = 
        let stringToParse = "These are some sample directions with added @salt for ~{5%minutes}."
        let expectedStep = Step {
            Directions = "These are some sample directions with added salt for 5 minutes."
            Equipment = []
            Ingredients = [ { Name = "salt" ; Amount = None } ]
            Timers = [ { Duration = float 5 ; Unit = "minutes" } ]
            Comment = ""
        }

        ParserTests.testParser Parser.step stringToParse expectedStep

module Equipment =
    [<Fact>]
    let ``A single-word equipment followed by a multi-word equipment is parsed correctly`` () = 
        let stringToParse = "These are some sample directions that require a #spatula and a #12-inch skillet{}."
        let expectedStep = Step {
            Directions = "These are some sample directions that require a spatula and a 12-inch skillet."
            Equipment = [ { Name = "spatula" } ; { Name = "12-inch skillet" } ]
            Ingredients = []
            Timers = []
            Comment = ""
        }

        ParserTests.testParser Parser.step stringToParse expectedStep

    [<Fact>]
    let ``A single-word equipment followed by a multi-word ingredient is parsed correctly`` () = 
        let stringToParse = "These are some sample directions using a #spoon to add @fenugreek leaves{}."
        let expectedStep = Step {
            Directions = "These are some sample directions using a spoon to add fenugreek leaves."
            Equipment = [ { Name = "spoon" } ]
            Ingredients = [ { Name = "fenugreek leaves" ; Amount = None } ]
            Timers = []
            Comment = ""
        }

        ParserTests.testParser Parser.step stringToParse expectedStep

    [<Fact>]
    let ``A single-word equipment followed by a timer is parsed correctly`` () = 
        let stringToParse = "These are some sample directions using a #spoon for ~{5%minutes}."
        let expectedStep = Step {
            Directions = "These are some sample directions using a spoon for 5 minutes."
            Equipment = [ { Name = "spoon" } ]
            Ingredients = []
            Timers = [ { Duration = float 5 ; Unit = "minutes" } ]
            Comment = ""
        }

        ParserTests.testParser Parser.step stringToParse expectedStep

module Comments =
    [<Fact>]
    let ``Single slashes should not be parsed as inline comments`` () = 
        let stringToParse = "This is a sample using a slash like this -> / to showcase that comments need two slashes and not one."
        let expectedStep = Step {
            Directions = "This is a sample using a slash like this -> / to showcase that comments need two slashes and not one."
            Equipment = []
            Ingredients = []
            Timers = []
            Comment = ""
        }

        ParserTests.testParser Parser.step stringToParse expectedStep

    [<Fact>]
    let ``Single slashes should not be parsed as comments`` () = 
        let stringToParse = "/ This is a sample using a slash to showcase that comments need two slashes and not one."
        let expectedStep = Step {
            Directions = "/ This is a sample using a slash to showcase that comments need two slashes and not one."
            Equipment = []
            Ingredients = []
            Timers = []
            Comment = ""
        }

        ParserTests.testParser Parser.step stringToParse expectedStep
        
module Metadata =  
    [<Fact>]
    let ``Single angle bracers should not be parsed as metadata`` () = 
        let stringToParse = "> This: is a sample using a single angle brace to showcase that metadata needs two and not one."
        let expectedStep = Step {
            Directions = "> This: is a sample using a single angle brace to showcase that metadata needs two and not one."
            Equipment = []
            Ingredients = []
            Timers = []
            Comment = ""
        }

        ParserTests.testParser Parser.step stringToParse expectedStep