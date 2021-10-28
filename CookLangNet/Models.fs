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

type Direction = {
    description: string
}

type Timer = {
    time: float
    unit: string
}

type Step = {
    timers: Timer list
    directions: Direction list
}

type Recipe = {
    steps: Step list
    ingredients: Ingredient list
    neededEquipment: Equipment list
    metadata: IDictionary<string, string>
}