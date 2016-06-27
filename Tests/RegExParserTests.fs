namespace RegExParserTests
open System
open NUnit.Framework
open RegularExpression
open RegularExpression.Parser


[<TestFixture>]
type RegExParser() = 
 static member validData =
        [
            // Escape tests

            // Concatination tests
        //    TestCaseData("a" |> List.ofSeq).Returns(Char('a')).SetName("a");
        //    TestCaseData("ab" |> List.ofSeq).Returns(Concatination(Char('a'), Char('b'))).SetName("ab");
        //    TestCaseData("abc" |> List.ofSeq).Returns(Concatination(Char('a'), Concatination(Char('b'), Char('c')))).SetName("abc");

            // Group tests
        //    TestCaseData("(a)" |> List.ofSeq).Returns(Group(Char('a'))).SetName("(a)");
         //   TestCaseData("(ab)" |> List.ofSeq).Returns(Group(Concatination(Char('a'), Char('b')))).SetName("(ab)");
         //   TestCaseData("(a(b))" |> List.ofSeq).Returns(Group(Concatination(Char('a'), Group(Char('b'))))).SetName("(a(b))")
         //   TestCaseData("(a)(b)" |> List.ofSeq).Returns(Concatination(Group(Char('a')), Group(Char('b')))).SetName("(a)(b)");

            // Choise tests
        //    TestCaseData("a|b" |> List.ofSeq).Returns(Choise(Char('a'), Char('b'))).SetName("a|b");
       //     TestCaseData("ac|bd" |> List.ofSeq).Returns(Choise(Concatination(Char('a'), Char('c')), Concatination(Char('b'), Char('d')))).SetName("ac|bd");
       //     TestCaseData("a|b|c" |> List.ofSeq).Returns(Choise(Char('a'), Choise(Char('b'), Char('c')))).SetName("a|b|c");

            // Group+Choise tests
        //    TestCaseData("(a)|(b)" |> List.ofSeq).Returns(Choise(Group(Char('a')), Group(Char('b')))).SetName("(a)|(b)");
        //    TestCaseData("(a|b)" |> List.ofSeq).Returns(Group(Choise(Char('a'), Char('b')))).SetName("(a|b)");

            // Star tests

            // Plus tests
        ]

   // [<Test>]
   // [<TestCaseSource(typedefof<RegExParser>, "validData")>]
   // member x.TestCase data =
     //   parse data


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
        Assert.Throws<System.Exception>(fun () -> parseSymbol [] |> ignore) |> ignore


    [<Test>]
    member x.UnsupportedSymbolThrows() =
        Assert.Throws<System.Exception>(fun () -> parseSymbol ['å'] |> ignore) |> ignore


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
