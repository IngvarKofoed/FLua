namespace RegularExpression
 
module Parser =
    // Generic used to handle circular dependency T is always Expression
    type Exp<'T> =
        | Union of left: 'T * right: Exp<'T> 
        | Expression of 'T
        | Empty
    
    type Symbol = 
        | Dot
        | Char of value: char
        | Escape of value: Symbol
    
    type Label =
        Symbol of value: Symbol
        
    type Term<'T> = 
        | Label of value: Label
        | Group of Exp<'T>
    
    type Expression = 
        | Concatination of left: Term<Expression> * right: Expression
        | Term of Term<Expression>
        | Star of Term<Expression>
        | Plus of Term<Expression>
        | Any of Term<Expression>


    let rec internal parseSymbol stream =
        match stream with
        | x::xs -> 
            match x with 
            | '.' -> 
                (Dot, xs)
            | '\\' -> 
                let (sym, rest) = parseSymbol xs
                (Escape(sym), rest)
            | c when c = 'a' -> 
                (Char(c), xs)
            | _ -> failwith "Unexpected symbol"
        | [] -> failwith "Symbol exspected"


    let internal parseLabel stream = // TODO: Consider injecting parseSymbol
        let (sym, rest) = parseSymbol stream
        (Symbol(sym), rest)


    let internal parseTerm stream (parseExp: char list -> Exp<Expression> * char list) =
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
        

    let rec internal parseExpression stream (parseExp: char list -> Exp<Expression> * char list) =
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


    let parse stream = 
        fst (parseExp stream)