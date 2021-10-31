namespace CookLangNet

open System.Collections.Generic

type UnitOfMeasurement = string

type IngredientAmount = {
    Quantity: float
    Unit: UnitOfMeasurement option
}

type Ingredient = {
    Name: string
    Amount: IngredientAmount option
}

type Equipment = {
    Name: string
}

type Timer = {
    Duration: float
    Unit: string
}

type Step = {
    Directions: string
    Timers: Timer list
    Ingredients: Ingredient list
    NeededEquipment: Equipment list
    Comment: string
}

type Recipe = {
    Steps: Step list
    Metadata: IDictionary<string, string>
}