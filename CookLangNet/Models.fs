namespace CookLangNet

open System.Text
open System.Collections.Generic


type UnitOfMeasurement = string

type IngredientAmount = {
    Quantity: float
    Unit: UnitOfMeasurement option
} 
with
    member this.Serialize() =
        let builder = StringBuilder("{").Append(this.Quantity.ToString())
        (match this.Unit with
        | Some unit -> builder.Append("%").Append(unit)
        | None -> builder).Append("}").ToString()


type Ingredient = {
    Name: string
    Amount: IngredientAmount option
}
with
    override this.ToString() = this.Name

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
            

type Equipment = {
    Name: string
}
with
    override this.ToString() = this.Name

    member this.Serialize() =
        StringBuilder("#")
            .Append(this.Name.ToString())
            .Append(if this.Name.Contains(" ") then "{}" else "")
            .ToString()

type Timer = {
    Duration: float
    Unit: string
}
with
    override this.ToString() = 
        StringBuilder(this.Duration.ToString())
            .Append(" ")
            .Append(this.Unit)
            .ToString()

    member this.Serialize() =
        StringBuilder("~{")
            .Append(this.Duration.ToString())
            .Append("%")
            .Append(this.Unit)
            .Append("}")
            .ToString()

type Step = {
    Directions: string
    Timers: Timer list
    Ingredients: Ingredient list
    NeededEquipment: Equipment list
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
    /// The parsed recipe.
    /// An exception containing details on what went wrong.
