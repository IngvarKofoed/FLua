namespace RegularExpression

open RegularExpression;
open RegularExpression.Evaluator

module ParseToNFA = 

    let parseSymbolToNFA symbol nextId = 
        match symbol with 
        | Parser.Dot -> failwith "Not implemented"
        | Parser.Char c -> [{ value = Char(c); fromNode = nextId; toNode = nextId + 1 }], nextId + 2
        | Parser.Escape s -> failwith "Not implemented"


    let parseLabelToNFA label nextId =
        match label with 
        | Parser.Symbol s -> parseSymbolToNFA s nextId


    let parseTermToNFA term nextId = 
        match term with 
        | Parser.Label l -> parseLabelToNFA l nextId
        | _ -> failwith "Not implemented"


    let first r = 
        List.head (fst r)

    let last r = 
        List.last (fst r)

    let replaceHead newHead r =
        newHead::(List.skip 1 (fst r))

    let concat l r =
        List.append (fst l) (fst r)

    let rec parseExpressionToNFA expression nextId =
        match expression with 
        | Parser.Concatination(l, r) -> 
            let left = parseTermToNFA l nextId 
            let right = parseExpressionToNFA r (snd left) 
            let leftLast = last left
            let rightFirst = first right
            let concatRight = { rightFirst with fromNode = leftLast.toNode }
            let newRight = (replaceHead concatRight right), (snd right)
            (concat left newRight), (snd right)
        | Parser.Term t -> parseTermToNFA t nextId
        | _ -> failwith "parseExpressionToNFA not implemented"

    let parseExpToNFA exp nextId = 
        match exp with
        | Parser.Union(_, _) -> failwith "parseExpToNFA not implemented"
        | Parser.Expression e -> parseExpressionToNFA e nextId 
        | Parser.Empty -> failwith "parseExpToNFA not implemented"

    let parseToNFA exp =
        fst (parseExpToNFA exp 0)
    
    let exp = Parser.Concatination(Parser.Label(Parser.Symbol(Parser.Char('a'))), Parser.Term(Parser.Label(Parser.Symbol(Parser.Char('b')))))

    let test = parseExpressionToNFA exp 0