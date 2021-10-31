module Generators

open FsCheck.Arb
open CookLangNet.Parser
open System.IO
open System.Text
open System.Text.RegularExpressions
open System
open FsCheck

type SingleWordNonWhiteSpaceString = SingleWordNonWhiteSpaceString of string with
    member x.Get = match x with SingleWordNonWhiteSpaceString s -> s
    override x.ToString() = x.Get

type SingleLineNonWhiteSpaceString = SingleLineNonWhiteSpaceString of string with
    member x.Get = match x with SingleLineNonWhiteSpaceString s -> s
    override x.ToString() = x.Get

type TrimmedSingleLineNonWhiteSpaceString = TrimmedSingleLineNonWhiteSpaceString of string with
    member x.Get = match x with TrimmedSingleLineNonWhiteSpaceString s -> s
    override x.ToString() = x.Get

type NormalPositiveFloat = NormalPositiveFloat of float with
    member x.Get = match x with NormalPositiveFloat f -> f
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

        builder.ToString()

let private truth x = true

let private removeSpaces (s: string) =
    Regex.Replace(s, @"\s+", "");

let private filterEmptyStrings s =
    not (String.IsNullOrWhiteSpace(s))

let private filterNanAndInfinities f =
    f <> Double.NaN && f <> Double.PositiveInfinity && f <> Double.NegativeInfinity

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

    static member NormalPositiveFloat () =
        Default.NormalFloat()
        |> mapFilter (float >> abs >> NormalFloat) truth
        |> convert (float >> NormalPositiveFloat) (string >> float >> NormalFloat)