// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
open RegularExpression.Evaluator

[<EntryPoint>]
let main argv = 

    let graph = [{ value = Char 'a'; fromNode = 0; toNode = 1 } ]
    
    let res = evaluateRegEx 0 graph (List.ofSeq "a")

   
    0 // return an integer exit code

