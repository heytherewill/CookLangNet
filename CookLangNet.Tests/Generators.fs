module Generators

open FsCheck.Arb
open System.IO
open System.Text

type SingleLineNonWhiteSpaceString = SingleLineNonWhiteSpaceString of string with
    member x.Get = match x with SingleLineNonWhiteSpaceString r -> r
    override x.ToString() = x.Get

let private transformIntoSingleLineString s =
    if s = null then 
        ""
    else 
        use reader = new StringReader(s)
        let builder = StringBuilder()

        let mutable shouldContinue = true

        while shouldContinue do 
            let linesRead = reader.ReadLine()
            shouldContinue <- linesRead <> null
            if linesRead <> null then
                builder.Append(linesRead) |> ignore

        builder.ToString()

let private filterEmptyStrings s =
    not (System.String.IsNullOrWhiteSpace(s))

type Default =
    static member SingleLineNonWhiteSpaceStringArb () =
        Default.String() 
        |> mapFilter transformIntoSingleLineString filterEmptyStrings

