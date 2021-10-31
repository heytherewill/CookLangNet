namespace CookLangNet

open System.Collections.Generic

type UnitOfMeasurement = string

type IngredientAmount = {
    quantity: float
    unit: UnitOfMeasurement option
}

type Ingredient = {
    name: string
    amount: IngredientAmount option
}

type Equipment = {
    name: string
}

type Timer = {
    duration: float
    unit: string
}

type Step = {
    directions: string
    timers: Timer list
    ingredients: Ingredient list
    neededEquipment: Equipment list
    comment: string
}

type Recipe = {
    steps: Step list
    metadata: IDictionary<string, string>
}