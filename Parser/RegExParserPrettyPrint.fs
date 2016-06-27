namespace RegularExpression


module PrettyPrint =

    let rec prettyPrintSymbol symbol =
        match symbol with 
        | Dot -> printf "."
        | Char c -> printf "%c" c
        | Escape s -> printf "\\"; prettyPrintSymbol s

    let prettyPrintLabel label = 
        match label with
        | Symbol s -> prettyPrintSymbol s 

    let rec prettyPrintTerm term prettyPrintExp =
        match term with
        | Label(l) -> prettyPrintLabel l
        | Group(exp) -> printf "("; prettyPrintExp exp; printf ")"


    let rec prettyPrintExpression expression prettyPrintExp = 
        match expression with
        | Concatination(l, r) -> prettyPrintTerm l prettyPrintExp; prettyPrintExpression r prettyPrintExp
        | Term(t) -> prettyPrintTerm t prettyPrintExp
        | Star(t) -> prettyPrintTerm t prettyPrintExp; printf "*"
        | Plus(t) -> prettyPrintTerm t prettyPrintExp; printf "+"
        | Any(t) -> prettyPrintTerm t prettyPrintExp; printf "?"

    let rec prettyPrintExp exp = 
        match exp with
        | Union(left, right) -> prettyPrintExpression left prettyPrintExp; printf "|"; prettyPrintExp right
        | Expression e -> prettyPrintExpression e prettyPrintExp
        | Empty -> ()
