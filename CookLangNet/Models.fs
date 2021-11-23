namespace CookLangNet

open System.Text
open System.Collections.Generic

/// Type alias for the unit of measurement of ingredients.
type UnitOfMeasurement = string

/// The amount of a given ingredient to use for a recipe.
type IngredientAmount = {
    /// Quantity of the ingredient.
    Quantity: float
    /// Optional unit of measurement.
    Unit: UnitOfMeasurement option
} 
with
    /// Turns this object into its string representation in CookLang.
    member this.Serialize() =
        let builder = StringBuilder("{").Append(this.Quantity.ToString())
        (match this.Unit with
        | Some unit -> builder.Append("%").Append(unit)
        | None -> builder).Append("}").ToString()

/// An ingredient used in a recipe.
type Ingredient = {
    /// The name of the ingredient.
    Name: string
    /// Optional amount of the ingredient to be used in the recipe.
    Amount: IngredientAmount option
}
with
    override this.ToString() = this.Name
    
    /// Turns this object into its string representation in CookLang.
    member this.Serialize() =
        StringBuilder("@")
            .Append(this.Name.ToString())
            .Append(
                match this.Amount with
                | Some amount -> amount.Serialize()
                | None -> 
                    if this.Name.Contains(" ") then "{}"
                    else "")
            .ToString()
            
/// An equipment needed for a recipe.
type Equipment = {
    /// The name of the equipment.
    Name: string
}
with
    override this.ToString() = this.Name
    
    /// Turns this object into its string representation in CookLang.
    member this.Serialize() =
        StringBuilder("#")
            .Append(this.Name.ToString())
            .Append(if this.Name.Contains(" ") then "{}" else "")
            .ToString()

/// A timer used in the recipe.
type Timer = {
    /// Name of the timer. This can be an empty string.
    Name: string
    /// Duration of the timer.
    Duration: float
    /// Unit of time of this timer.
    Unit: string
}
with
    override this.ToString() = 
        StringBuilder(this.Duration.ToString())
            .Append(" ")
            .Append(this.Unit)
            .ToString()

    /// Turns this object into its string representation in CookLang.
    member this.Serialize() =
        StringBuilder("~")
            .Append(this.Name)
            .Append("{")
            .Append(this.Duration.ToString())
            .Append("%")
            .Append(this.Unit)
            .Append("}")
            .ToString()

/// A step in the preparation of a recipe.            
type Step = {
    /// Directions of this step.
    Directions: string
    /// Timers used in this step.
    Timers: Timer list
    /// Ingredients used in this step.
    Ingredients: Ingredient list
    /// Equipment used in this step.
    Equipment: Equipment list
    /// Any additional comments for this step.
    Comment: string
}

/// A recipe.
type Recipe = {
    /// Steps needed to prepare the recipe.
    Steps: Step list
    /// Additional metadata.
    Metadata: IDictionary<string, string>
}

/// Result of parsing a CookLang recipe.
type CookLangParserResult =
    /// The parsed recipe.
    | Success of Recipe
    /// An exception containing details on what went wrong.
    | Failure of string