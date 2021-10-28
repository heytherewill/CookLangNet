module Generators

open FsCheck.Arb
open CookLangNet.Parser
open System.IO
open System.Text
open System.Text.RegularExpressions

type SingleWordNonWhiteSpaceString = SingleWordNonWhiteSpaceString of string with
    member x.Get = match x with SingleWordNonWhiteSpaceString s -> s
    override x.ToString() = x.Get

type SingleLineNonWhiteSpaceString = SingleLineNonWhiteSpaceString of string with
    member x.Get = match x with SingleLineNonWhiteSpaceString s -> s
    override x.ToString() = x.Get

type TrimmedSingleLineNonWhiteSpaceString = TrimmedSingleLineNonWhiteSpaceString of string with
    member x.Get = match x with TrimmedSingleLineNonWhiteSpaceString s -> s
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

let private removeSpaces (s: string) =
    Regex.Replace(s, @"\s+", "");

let private transformIntoSingleWordString = 
    transformIntoSingleLineString >> removeSpaces

let private filterEmptyStrings s =
    not (System.String.IsNullOrWhiteSpace(s))

type Default =
    static member SingleLineNonWhiteSpaceString () =
        Default.String() 
        |> mapFilter transformIntoSingleLineString filterEmptyStrings
        |> convert SingleLineNonWhiteSpaceString string

    static member TrimmedSingleLineNonWhiteSpaceString () =
        Default.String() 
        |> mapFilter (transformIntoSingleLineString >> trim) filterEmptyStrings
        |> convert TrimmedSingleLineNonWhiteSpaceString string

    static member SingleWordNonWhiteSpaceString () =
        Default.String() 
        |> mapFilter (transformIntoSingleLineString >> removeSpaces) filterEmptyStrings
        |> convert SingleWordNonWhiteSpaceString string