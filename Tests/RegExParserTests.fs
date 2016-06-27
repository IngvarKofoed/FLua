namespace RegExParserTests
open System
open NUnit.Framework
open RegularExpression
open RegularExpression.Parser

[<TestFixture>]
type RegExParser_parseSymbol() = 
    static member validData =
       [
           TestCaseData("." |> List.ofSeq).Returns((Dot, List.empty<char>)).SetName(".");
           TestCaseData("a" |> List.ofSeq).Returns((Char('a'), List.empty<char>)).SetName("a");
           TestCaseData("\\." |> List.ofSeq).Returns((Escape(Dot), List.empty<char>)).SetName("\\.");
       ]

    [<Test>]
    [<TestCaseSource(typedefof<RegExParser_parseSymbol>, "validData")>]
    member x.ValidCases data =
        parseSymbol data

    [<Test>]
    member x.EmptyStreamThrows() =
        Assert.Throws<Exception>(fun () -> parseSymbol [] |> ignore) |> ignore


    [<Test>]
    member x.UnsupportedSymbolThrows() =
        Assert.Throws<Exception>(fun () -> parseSymbol ['å'] |> ignore) |> ignore


[<TestFixture>]
type RegExParser_parseLabel() = 
    static member validData =
       [
           TestCaseData("a" |> List.ofSeq).Returns((Symbol(Char('a')), List.empty<char>)).SetName("a");
       ]

    [<Test>]
    [<TestCaseSource(typedefof<RegExParser_parseLabel>, "validData")>]
    member x.ValidCases data =
        parseLabel data
