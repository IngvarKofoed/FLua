// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
open RegularExpression

[<EntryPoint>]
let main argv = 

    let ast = Parser.parse (List.ofSeq "abc")
    let nfa = ParseToNFA.parseToNFA ast
    let result = Evaluator.evaluate nfa (List.ofSeq "abc") 
   
    0 // return an integer exit code

