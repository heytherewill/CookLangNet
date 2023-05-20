namespace CookLangNet.CSharp

open System.Collections.Generic
open System

type private FSharpRecipe = CookLangNet.Recipe
type private FSharpStep = CookLangNet.Step
type private FSharpTimer = CookLangNet.Timer
type private FSharpCookware = CookLangNet.Cookware
type private FSharpIngredient = CookLangNet.Ingredient

type IngredientAmount = {
    Quantity: string
    Unit: string
}
with 
    member this.TryNumericQuantity(numeric: Ref<float>) =
        System.Double.TryParse(this.Quantity, numeric)

type Ingredient = {
    Name: string
    Amount: IngredientAmount
}

type Cookware = {
    Name: string
    Quantity: string
}
with 
    member this.TryNumericQuantity(numeric: Ref<float>) =
        System.Double.TryParse(this.Quantity, numeric)

type Timer = {
    Name: string
    Duration: float
    Unit: string 
}

type Step = {
    Directions: string
    Timers: List<Timer>
    Ingredients: List<Ingredient>
    Cookware: List<Cookware>
}

type Recipe = {
    Steps: List<Step> 
    Metadata: IDictionary<string, string> 
}

type CookLangParserException(errorMessage: string) =
    inherit Exception(errorMessage)

module CookLangParser =

    let private toCSharpTimer (timer: FSharpTimer) = 
        { Name = timer.Name ; Duration = timer.Duration ; Unit = timer.Unit }

    let private toCSharpIngredient (ingredient: FSharpIngredient) = 
        { 
            Name = ingredient.Name ;
            Amount = 
                match ingredient.Amount with
                | None -> Unchecked.defaultof<IngredientAmount>
                | Some amount -> { Quantity = amount.Quantity.Serialize() ; Unit = amount.Unit |> Option.defaultValue "" } }

    let private toCSharpCookware (cookware: FSharpCookware) = 
        { Name = cookware.Name ; Quantity = cookware.Quantity.Serialize() }

    let private toCSharpStep (step: FSharpStep) =
        { 
            Directions = step.Directions;
            Timers = step.Timers |> Seq.map toCSharpTimer |> List<Timer> ;
            Ingredients = step.Ingredients|> Seq.map toCSharpIngredient |> List<Ingredient> ;
            Cookware = step.Cookware |> Seq.map toCSharpCookware |> List<Cookware>
        }

    let private toCSharpRecipe (recipe: FSharpRecipe) =
        {
            Steps = recipe.Steps |> Seq.map toCSharpStep |> List<Step> ;
            Metadata = recipe.Metadata
        }

    ///<summary>Parses the passed string into a Recipe.</summary>
    ///<param name="stringToParse">The input string.</param>
    ///<returns>The parsed recipe.</returns>
    ///<exception cref="CookLangNet.CSharp.CookLangParserException">Thrown when the input is an invalid Recipe.</exception>
    let Parse (stringToParse: string) =
        match CookLangNet.CookLangParser.parse(stringToParse) with
        | CookLangNet.CookLangParserResult.Success recipe -> toCSharpRecipe recipe
        | CookLangNet.CookLangParserResult.Failure message -> CookLangParserException(message) |> raise