﻿module RegressionTests

(* These test bugs in the code that were fixed to prevent they from sneaking back in. *)

open CookLangNet
open CookLangNet.Parser
open Xunit

module Ingredients =
    [<Fact>]
    let ``A single-word ingredient followed by a multi-word ingredient is parsed correctly`` () =
        let stringToParse =
            "These are some sample directions with added @salt and @black pepper{}."

        let expectedStep =
            Step
                { Directions = "These are some sample directions with added salt and black pepper."
                  Cookware = []
                  Ingredients = [ { Name = "salt"; Amount = None }; { Name = "black pepper"; Amount = None } ]
                  Timers = []
                //Comment = ""
                }

        ParserTests.testParser Parser.step stringToParse expectedStep

    [<Fact>]
    let ``A single-word ingredient followed by a multi-word cookware is parsed correctly`` () =
        let stringToParse =
            "These are some sample directions with added @salt via a #salt shaker{}."

        let expectedStep =
            Step
                { Directions = "These are some sample directions with added salt via a salt shaker."
                  Cookware =
                    [ { Name = "salt shaker"
                        Quantity = Numeric 1 } ]
                  Ingredients = [ { Name = "salt"; Amount = None } ]
                  Timers = []
                //Comment = ""
                }

        ParserTests.testParser Parser.step stringToParse expectedStep

    [<Fact>]
    let ``A single-word ingredient followed by a timer is parsed correctly`` () =
        let stringToParse =
            "These are some sample directions with added @salt for ~{5%minutes}."

        let expectedStep =
            Step
                { Directions = "These are some sample directions with added salt for 5 minutes."
                  Cookware = []
                  Ingredients = [ { Name = "salt"; Amount = None } ]
                  Timers =
                    [ { Name = ""
                        Duration = float 5
                        Unit = "minutes" } ]
                //Comment = ""
                }

        ParserTests.testParser Parser.step stringToParse expectedStep

module Cookware =
    [<Fact>]
    let ``A single-word cookware followed by a multi-word cookware is parsed correctly`` () =
        let stringToParse =
            "These are some sample directions that require a #spatula and a #12-inch skillet{}."

        let expectedStep =
            Step
                { Directions = "These are some sample directions that require a spatula and a 12-inch skillet."
                  Cookware =
                    [ { Name = "spatula"
                        Quantity = Numeric 1 }
                      { Name = "12-inch skillet"
                        Quantity = Numeric 1 } ]
                  Ingredients = []
                  Timers = []
                //Comment = ""
                }

        ParserTests.testParser Parser.step stringToParse expectedStep

    [<Fact>]
    let ``A single-word cookware followed by a multi-word ingredient is parsed correctly`` () =
        let stringToParse =
            "These are some sample directions using a #spoon to add @fenugreek leaves{}."

        let expectedStep =
            Step
                { Directions = "These are some sample directions using a spoon to add fenugreek leaves."
                  Cookware = [ { Name = "spoon"; Quantity = Numeric 1 } ]
                  Ingredients =
                    [ { Name = "fenugreek leaves"
                        Amount = None } ]
                  Timers = []
                //Comment = ""
                }

        ParserTests.testParser Parser.step stringToParse expectedStep

    [<Fact>]
    let ``A single-word cookware followed by a timer is parsed correctly`` () =
        let stringToParse =
            "These are some sample directions using a #spoon for ~{5%minutes}."

        let expectedStep =
            Step
                { Directions = "These are some sample directions using a spoon for 5 minutes."
                  Cookware = [ { Name = "spoon"; Quantity = Numeric 1 } ]
                  Ingredients = []
                  Timers =
                    [ { Name = ""
                        Duration = float 5
                        Unit = "minutes" } ]
                //Comment = ""
                }

        ParserTests.testParser Parser.step stringToParse expectedStep

module Comments =
    [<Fact>]
    let ``Single slashes should not be parsed as inline comments`` () =
        let stringToParse =
            "This is a sample using a slash like this -> / to showcase that comments need two slashes and not one."

        let expectedStep =
            Step
                { Directions =
                    "This is a sample using a slash like this -> / to showcase that comments need two slashes and not one."
                  Cookware = []
                  Ingredients = []
                  Timers = []
                //Comment = ""
                }

        ParserTests.testParser Parser.step stringToParse expectedStep

    [<Fact>]
    let ``Single dashes should not be parsed as comments`` () =
        let stringToParse =
            "- This is a sample using a slash to showcase that comments need two slashes and not one."

        let expectedStep =
            Step
                { Directions =
                    "- This is a sample using a slash to showcase that comments need two slashes and not one."
                  Cookware = []
                  Ingredients = []
                  Timers = []
                //Comment = ""
                }

        ParserTests.testParser Parser.step stringToParse expectedStep

module Metadata =
    [<Fact>]
    let ``Single angle bracers should not be parsed as metadata`` () =
        let stringToParse =
            "> This: is a sample using a single angle brace to showcase that metadata needs two and not one."

        let expectedStep =
            Step
                { Directions =
                    "> This: is a sample using a single angle brace to showcase that metadata needs two and not one."
                  Cookware = []
                  Ingredients = []
                  Timers = []
                //Comment = ""
                }

        ParserTests.testParser Parser.step stringToParse expectedStep
