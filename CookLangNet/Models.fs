namespace CooklangNet

open System.Collections.Generic

type UnitOfMeasurement = string

type IngredientAmount = {
    quantity: float
    unit: UnitOfMeasurement
}

type Ingredient = {
    name: string
    amount: IngredientAmount
}

type Equipment = {
    name: string
}

type Direction = {
    description: string
}

type Timer = {
    quantity: float
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