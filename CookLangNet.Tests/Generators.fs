module Generators

open FsCheck.Arb
open CookLangNet
open System.IO
open System.Text
open System.Text.RegularExpressions
open System
open FsCheck

type SingleWordString = SingleWordString of string with
    member x.Get = match x with SingleWordString s -> s
    override x.ToString() = x.Get

type SingleLineString = SingleLineString of string with
    member x.Get = match x with SingleLineString s -> s
    override x.ToString() = x.Get

type TrimmedSingleLineString = TrimmedSingleLineString of string with
    member x.Get = match x with TrimmedSingleLineString s -> s
    override x.ToString() = x.Get

type NormalPositiveFloat = NormalPositiveFloat of float with
    member x.Get = match x with NormalPositiveFloat f -> f
    override x.ToString() = x.Get.ToString()

type SingleWordCookware = SingleWordCookware of Cookware with
    member x.Get = match x with SingleWordCookware e -> e
    member x.Serialize() = x.Get.Serialize()
    override x.ToString() = x.Get.ToString()

type MultiWordCookware = MultiWordCookware of Cookware with
    member x.Get = match x with MultiWordCookware e -> e
    member x.Serialize() = x.Get.Serialize()
    override x.ToString() = x.Get.ToString()
    
type SingleWordNoAmountIngredient = SingleWordNoAmountIngredient of Ingredient with
    member x.Get = match x with SingleWordNoAmountIngredient e -> e
    member x.Serialize() = x.Get.Serialize()
    override x.ToString() = x.Get.ToString()
    
type MultiWordNoAmountIngredient = MultiWordNoAmountIngredient of Ingredient with
    member x.Get = match x with MultiWordNoAmountIngredient e -> e
    member x.Serialize() = x.Get.Serialize()
    override x.ToString() = x.Get.ToString()
    
type SingleWordWithAmountIngredient = SingleWordWithAmountIngredient of Ingredient with
    member x.Get = match x with SingleWordWithAmountIngredient e -> e
    member x.Serialize() = x.Get.Serialize()
    override x.ToString() = x.Get.ToString()
    
type MultiWordWithAmountIngredient = MultiWordWithAmountIngredient of Ingredient with
    member x.Get = match x with MultiWordWithAmountIngredient e -> e
    member x.Serialize() = x.Get.Serialize()
    override x.ToString() = x.Get.ToString()
    
type ValidTimer = ValidTimer of Timer with
    member x.Get = match x with ValidTimer e -> e
    member x.Serialize() = x.Get.Serialize()
    override x.ToString() = x.Get.ToString()

let private transformIntoSingleLineString s =
    if isNull s then 
        ""
    else 
        use reader = new StringReader(s)
        let builder = StringBuilder()

        let mutable shouldContinue = true

        while shouldContinue do 
            let linesRead = reader.ReadLine()
            shouldContinue <- not <| isNull linesRead 
            if not <| isNull linesRead then
                builder.Append(linesRead) |> ignore

        /// Removes characters that would cause the individual 
        /// units not to be parsed as expected.
        Regex.Replace(builder.ToString(), @"({|}|#|@|,|\.|~|%)+", "")

let private truth _ = true
let private removeSpaces (s: string) = Regex.Replace(s, @"\s+", "");
let private filterEmptyStrings s = not (String.IsNullOrWhiteSpace(s))

let toAmountOption shouldBeSome quantity shouldContainUnit unit = 
    let unit = if shouldContainUnit then Some unit else None
    if shouldBeSome then Some { Quantity = quantity; Unit = unit } else None

let toCookware includeQuantity name quantity = { Name = name; Quantity = if includeQuantity then Some quantity else None }
let toIngredient name amount = { Name = name; Amount = amount }
let toTimer name duration unit = { Name = name; Duration = duration; Unit = unit }
let stringFunc = Func<_,_>(string)

type Default =
    static member SingleLineString () =
        Default.String() 
        |> mapFilter transformIntoSingleLineString filterEmptyStrings
        |> convert SingleLineString string

    static member TrimmedSingleLineString () =
        Default.String() 
        |> mapFilter (transformIntoSingleLineString >> trim) filterEmptyStrings
        |> convert TrimmedSingleLineString string

    static member SingleWordString () =
        Default.String() 
        |> mapFilter (transformIntoSingleLineString >> removeSpaces) filterEmptyStrings
        |> convert SingleWordString string
        
    static member NormalPositiveFloat () =
        Default.NormalFloat()
        |> mapFilter (float >> abs >> NormalFloat) truth
        |> convert (float >> NormalPositiveFloat) (string >> float >> NormalFloat)

    static member AmountOption () =
        let bool = Default.Bool().Generator
        let quantity = Default.NormalPositiveFloat().Generator.Select(fun x -> x.Get)
        let unit = Default.SingleWordString().Generator.Select(stringFunc)
        Gen.map4 toAmountOption bool quantity bool unit |> Arb.fromGen

    static member MultiWordCookware () =
        let bool = Default.Bool().Generator
        let name = Default.SingleLineString().Generator.Select(stringFunc)
        let quantity = Default.SingleLineString().Generator.Select(fun x -> x.Get)
        Gen.map3 toCookware bool name quantity |> Gen.map MultiWordCookware |> Arb.fromGen

    static member SingleWordCookware () =
        let bool = Default.Bool().Generator
        let name = Default.SingleWordString().Generator.Select(stringFunc)
        let quantity = Default.SingleLineString().Generator.Select(fun x -> x.Get)
        Gen.map3 toCookware bool name quantity |> Gen.map SingleWordCookware |> Arb.fromGen
        
    static member MultiWordNoAmountIngredient () =
        let name = Default.SingleLineString().Generator.Select(stringFunc)
        Gen.map2 toIngredient name (Gen.constant None) |> Gen.map MultiWordNoAmountIngredient |> Arb.fromGen
        
    static member SingleWordNoAmountIngredient () =
        let name = Default.SingleWordString().Generator.Select(stringFunc)
        Gen.map2 toIngredient name (Gen.constant None) |> Gen.map SingleWordNoAmountIngredient |> Arb.fromGen
                
    static member MultiWordWithAmountIngredient () =
        let name = Default.SingleLineString().Generator.Select(stringFunc)
        let amount = Default.AmountOption().Generator
        Gen.map2 toIngredient name amount |> Gen.map MultiWordWithAmountIngredient |> Arb.fromGen
        
    static member SingleWordWithAmountIngredient () =
        let name = Default.SingleWordString().Generator.Select(stringFunc)
        let amount = Default.AmountOption().Generator
        Gen.map2 toIngredient name amount |> Gen.map SingleWordWithAmountIngredient |> Arb.fromGen
        
    static member ValidTimer () =
        let name = Default.SingleWordString().Generator.Select(stringFunc)
        let duration = Default.NormalPositiveFloat().Generator.Select(fun x -> x.Get)
        let unit = Default.SingleWordString().Generator.Select(stringFunc)
        Gen.map3 toTimer name duration unit |> Gen.map ValidTimer |> Arb.fromGen