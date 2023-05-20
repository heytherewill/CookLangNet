namespace CookLangNet

open System.Text
open System.Collections.Generic

/// Type alias for the unit of measurement of ingredients.
type UnitOfMeasurement = string

type Quantity =
    | Numeric of float
    | Textual of string

    /// Turns this object into its string representation in CookLang.
    member this.Serialize() =
        match this with
        | Textual s -> s
        | Numeric n -> n.ToString()

/// The amount of a given ingredient to use for a recipe.
type IngredientAmount =
    {
        /// Quantity of the ingredient.
        Quantity: Quantity
        /// Optional unit of measurement.
        Unit: UnitOfMeasurement option
    }

    /// Turns this object into its string representation in CookLang.
    member this.Serialize() =
        let builder = StringBuilder("{").Append(this.Quantity.Serialize())

        (match this.Unit with
         | Some unit -> builder.Append("%").Append(unit)
         | None -> builder)
            .Append("}")
            .ToString()

/// An ingredient used in a recipe.
type Ingredient =
    {
        /// The name of the ingredient.
        Name: string
        /// Optional amount of the ingredient to be used in the recipe.
        Amount: IngredientAmount option
    }

    override this.ToString() = this.Name

    /// Turns this object into its string representation in CookLang.
    member this.Serialize() =
        StringBuilder("@")
            .Append(this.Name.ToString())
            .Append(
                match this.Amount with
                | Some amount -> amount.Serialize()
                | None -> if this.Name.Contains(" ") then "{}" else ""
            )
            .ToString()

/// An piece of cookware needed for a recipe.
type Cookware =
    {
        /// The name of the cookware.
        Name: string
        /// The quantity of the cookware needed for the recipe.
        Quantity: Quantity
    }

    override this.ToString() = this.Name

    /// Turns this object into its string representation in CookLang.
    member this.Serialize() =
        StringBuilder("#")
            .Append(this.Name.ToString())
            .Append(
                match this.Quantity with
                | Textual _ -> "{" + this.Quantity.Serialize() + "}"
                | Numeric n -> "{" + (if n = 1.0 then "" else this.Quantity.Serialize()) + "}"
            )
            .ToString()

/// A timer used in the recipe.
type Timer =
    {
        /// Name of the timer. This can be an empty string.
        Name: string
        /// Duration of the timer.
        Duration: float
        /// Unit of time of this timer.
        Unit: string
    }

    override this.ToString() =
        StringBuilder(this.Duration.ToString()).Append(" ").Append(this.Unit).ToString()

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
type Step =
    {
        /// Directions of this step.
        Directions: string
        /// Timers used in this step.
        Timers: Timer list
        /// Ingredients used in this step.
        Ingredients: Ingredient list
        /// Cookware used in this step.
        Cookware: Cookware list
    }

/// A recipe.
type Recipe =
    {
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
