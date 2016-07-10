#if COMPILED
module BRParser
#endif

    type ParserResult<'Result> = 
        | Success of 'Result
        | Failure of string
    
    type Reply<'Result> = 
        | Match of 'Result
        | NoMatch of string
        | Error of string
    
    type CharStream(text : string) = 
        let mutable position = 0
        member this.text = text
        member this.Position = position
        member this.Peek = 
            if position < String.length text then text.[position]
            else '\uFFFF'
        member this.Skip = 
            if position < String.length text then position <- position + 1
        member this.SkipChar c = 
            if position < String.length text then
                let rc = this.Read
                rc = c
            else 
                false
        member this.BacktrackTo pos = position <- pos
        member this.Read =
            if position < String.length text then 
                let c = this.Peek
                position <- position + 1
                c
            else 
                '\uFFFF'
    
    type Parser<'Result> = CharStream -> Reply<'Result>
    
    
    /// Matches the given char c
    let char c =
        fun (stream: CharStream) ->
            if stream.Peek = c then
                stream.Skip
                Match c
            else
                NoMatch (sprintf "%c expected" c)
    
    let inRange value lower upper = value <= upper && lower <= value
    let isAsciiLower c = inRange c 'a' 'z'
    let isAsciiUpper c = inRange c 'A' 'Z'
    
    let asciiLower = 
        fun (stream : CharStream) -> 
            let c = stream.Peek
            if isAsciiLower c then 
                stream.Skip
                Match c
            else NoMatch "a-z expected"
    
    /// Matches any letter [a-Z]
    let letter = 
        fun (stream : CharStream) -> 
            let c = stream.Peek
            if isAsciiLower c || isAsciiUpper c then 
                stream.Skip
                Match c
            else NoMatch "a-> expected"
    
    let isDigit dig = inRange dig '0' '9'
    
    /// Matches a single digit
    let digit = 
        fun (stream : CharStream) -> 
            let c = stream.Peek
            if isDigit c then 
                stream.Skip
                Match c
            else NoMatch "0-9 expected"
    
    /// Matches the give string s
    let pstring s = 
        fun (stream : CharStream) -> 
            let pos = stream.Position
            let res = seq { 1..String.length s } |> Seq.tryFind (fun i -> not (stream.SkipChar s.[i - 1]))
            match res with
            | None -> Match s
            | Some pos -> 
                stream.BacktrackTo pos
                NoMatch (sprintf "%s expected" s)  // TODO: Use pos for error position (Or do it just work)
    
    /// Matches a integer number that can have leading zeroes
    let pint32 =
        fun (stream: CharStream) ->
            let pos = stream.Position
            let rec readDigits (stm: CharStream) (res: string) = 
                let c = stm.Peek
                if isDigit c then
                    stm.Skip 
                    readDigits stm (res + string(c))
                else res
    
            let result = readDigits stream ""
            if String.length result > 0 then
                Match (System.Int32.Parse result)
            else
                stream.BacktrackTo pos 
                NoMatch "int32 expected"
    
    /// Matches any number of white spaces
    let spaces =
        fun (stream: CharStream) ->
            let rec eater (stm: CharStream) = 
                if stm.Peek = ' ' then 
                    stm.Skip
                    eater stm
                else 
                    ()
            eater stream
            Match ()
    
    ////////////////////
    
    /// Choise: Runs p1 and if that is Match then this is returned, else the result of running p2 is returned.
    let (<|>) (p1 : Parser<'Result>) (p2 : Parser<'Result>) = 
        fun (stream: CharStream) -> 
            let result = p1 stream
            match result with
            | Match _ -> result
            | _ -> p2 stream
    
    
    /// Concat: Runs p1 and if that is a Match it runs p2 and returns their values as a tuple.
    let (.>>.) (p1 : Parser<'Result1>) (p2 : Parser<'Result2>) = 
        fun (stream: CharStream) ->
            let result1 = p1 stream
            match result1 with
            | Match r1 ->
                let result2 = p2 stream
                match result2 with
                | Match r2 -> Match (r1, r2) 
                | _ -> NoMatch "??????????"
            | _ -> NoMatch "??????????"
    
    
    /// Concat: Runs p1 and if that is a Match, then runs p2 and returns the result of running p1.
    let (.>>) (p1 : Parser<'Result1>) (p2 : Parser<'Result2>) = 
        fun (stream: CharStream) ->
            let result1 = p1 stream
            match result1 with
            | Match _ ->
                let result2 = p2 stream
                match result2 with
                | Match _ -> result1
                | _ -> NoMatch "??????????"
            | _ -> result1
    
    /// Map: Runs p and if that is a Match, then applies the map to the result.
    let (|>>) (p: Parser<'a>) (map: 'a -> 'b) =
        fun (stream: CharStream) -> 
            let res = p stream
            match res with
            | Match v -> Match (map v)
            | NoMatch s -> NoMatch s
            | Error s -> Error s

    /// Attempt: Runs p and if that is not a match, then it backtracks and returns the result of p.
    let attempt (p : Parser<'Result>) = 
        fun (stream : CharStream) -> 
            let pos = stream.Position
            let reply = p stream
            match reply with
            | Match _ -> reply
            | _ -> 
                stream.BacktrackTo pos
                reply
    
    ////////////////////
    
    let createParserForwardedToRef() =
        let dummyParser = fun (stream: CharStream) -> failwith "Ref parser not initialized!"
        let r = ref dummyParser
        (fun (stream: CharStream) -> !r stream), r : Parser<_> * Parser<_> ref
    
    ////////////////////
    
    let run parser (stream : CharStream) = 
        let rec spaces p s = 
            if p = 0 then s
            else spaces (p - 1) (" " + s)
        
        let result = parser stream
        match result with
        | Match _ -> Success result
        | NoMatch s -> Failure (sprintf "%s\n%s\n%s\n" s stream.text (spaces stream.Position "^"))
        | Error _ -> Failure ""
    
    let print r = 
        match r with
        | Success r -> printf "Success: %A\n" r
        | Failure m -> printf "Failure:\n%s" m
    
    
     
    
    ////////////////////////////////////
    

    
    
    
    type Op =
        | Add
        | Sub
        
    type Exp =
        | Number of int
        | BinOp of Exp * Op * Exp

    
    let add = char '+' |>> fun _ -> Add
    let sub = char '-'|>> fun _ -> Sub
    
    let binOp = add <|> sub
    
    let number = pint32 .>> spaces |>> fun x -> Number x

    let lhExp = number // Left recursion prevention
    let exp, expImpl = createParserForwardedToRef() // Forward recusion
    
    let binExp = lhExp .>>. binOp .>>. exp |>> fun ((l, o), r) -> BinOp(l, o, r)
      
    do expImpl := attempt binExp <|> number

    run exp (CharStream("1-2+3+4-5")) |> print 


    type MyTest =
        | Bla of int
        | Kurt of Hest
    and Hest =
        | Flemming

 