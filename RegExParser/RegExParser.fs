namespace RegularExpression
 
module Parser =
 
    type Exp =
        | Union of left: Expression * right: Exp 
        | Expression of Expression
        | Empty
    and Expression = 
        | Concatination of left: Term * right: Expression
        | Term of Term
        | Star of Term
        | Plus of Term
        | Any of Term
    and Term = 
        | Label of value: Label
        | Group of Exp
    and Label =
        Symbol of value: Symbol
    and Symbol = 
        | Dot
        | Char of value: char
        | Escape of value: Symbol
    
   

    let private symbols = List.ofSeq "abcdefghijklmnopqrstuvwz"

    let rec internal parseSymbol stream =
        match stream with
        | x::xs -> 
            match x with 
            | '.' -> 
                (Dot, xs)
            | '\\' -> 
                let (sym, rest) = parseSymbol xs
                (Escape(sym), rest)
            | c when List.contains c symbols ->
                (Char(c), xs)
            | _ -> failwith "Unexpected symbol"
        | [] -> failwith "Symbol exspected"


    let internal parseLabel stream = // TODO: Consider injecting parseSymbol
        let (sym, rest) = parseSymbol stream
        (Symbol(sym), rest)


    let internal parseTerm stream (parseExp: char list -> Exp * char list) =
        match stream with 
        | x::xs ->
            match x with 
            | '(' ->
                let (exp, rest) = parseExp xs
                let consumed = List.skip 1 rest
                (Group(exp), consumed)
            | _ ->
                let (lab, rest) = parseLabel stream
                (Label(lab), rest)
        | _ -> failwith "Term expected"
        

    let rec internal parseExpression stream (parseExp: char list -> Exp * char list) =
        let (term, rest) = parseTerm stream parseExp
        match rest with
        | x::xs ->
            match x with  
            | '*' -> (Star(term), xs)
            | '+' -> (Plus(term), xs)
            | '?' -> (Any(term), xs)
            | c when c <> ')' && c <> '|' ->
                let (right, rst) = parseExpression rest parseExp
                (Concatination(term, right), rst)
            | _ -> (Term(term), rest)
        | [] -> (Term(term), rest)


    let rec internal parseExp stream =
        match stream with
        | [] -> (Empty, [])
        | _ ->
            let (expression, rest) = parseExpression stream parseExp
            match rest with
            | [] -> (Expression(expression), rest)
            | x::xs ->
                match x with
                | '|' ->
                    let (right, rst) = parseExp xs
                    (Union(expression, right), rst)
                | ')' -> 
                    (Expression(expression), rest)
                | _ ->
                    failwith "parseExp"


    // Test
    /// <param name="stream">description</param>
    let parse stream = 
        fst (parseExp stream)