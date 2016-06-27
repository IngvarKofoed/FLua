// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
open RegularExpression
open RegularExpression.Parser

[<EntryPoint>]
let main argv = 

    //PrettyPrint.prettyPrintSymbol Dot
    let res = parseSymbol (List.ofSeq "a")
     
    PrettyPrint.prettyPrintSymbol (fst res)


   
    0 // return an integer exit code

