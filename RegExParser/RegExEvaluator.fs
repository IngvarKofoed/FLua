namespace RegularExpression

module Evaluator =

    type Node = 
        int
    
    type TransitionValue =
        | Empty
        | Char of char
    
    type Transition = {
        value: TransitionValue;
        fromNode: Node;
        toNode: Node 
    }
    
    type FAGraph =
        Transition list


    let rec evaluateRegEx current graph stream =
        match stream with
        | [] -> 
            let maxTrans = List.maxBy (fun t -> t.toNode) graph
            maxTrans.toNode = current // If this is not true, we are not done matching
        | x::xs ->
            let transitions = List.where (fun t -> t.fromNode = current && (t.value = Empty || t.value = Char x)) graph
            match transitions with 
            | [] -> false // Did not match, failed to match char x
            | [t] -> evaluateRegEx t.toNode graph xs
            | _ -> failwith "Non deterministic finite automata"      

    let evaluate graph stream =
        evaluateRegEx 0 graph stream
      

