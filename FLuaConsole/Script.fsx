// Very strongly inspired by the project: http://www.quanttec.com/fparsec/


type Stream = char list


type Result<'a> =
    | Success of result: 'a * stream: Stream
    | Failure


type Parser<'a> = Parser of parser: (Stream -> Result<'a>)


let runParser parser stream =
    let Parser (parser = parserFunc) = parser
    parserFunc stream

let returnParser x =
    let inner stream =
        Success(x, stream)
    Parser inner

let orParser parser1 parser2 = 
    let inner stream = 
        let result1 = runParser parser1 stream
        match result1 with
        | Success _ -> result1
        | Failure ->
            let result2 = runParser parser2 stream
            result2
    Parser inner


let ( <|> ) = orParser


let andParser parser1 parser2 =
    let inner stream =
        let result1 = runParser parser1 stream
        match result1 with 
        | Success (res1, rest1) ->
            let result2 = runParser parser2 rest1
            match result2 with
            | Success (res2, rest2) -> Success((res1, res2), rest2)
            | Failure -> Failure
        | Failure -> Failure
    Parser inner

 
let ( .>>. ) = andParser



let rec choise parsers =
    let inner stream =
        match parsers with
        | x::xs -> 
            let result = runParser x stream
            match result with
            | Success _ -> result
            | Failure -> runParser (choise xs) stream
        | _ -> Failure
    Parser inner


let bindParser parser func =
    let inner stream =
        let result = runParser parser stream
        match result with 
        | Success (res, rest) -> 
            let fParser = func res
            runParser fParser rest
        | Failure -> Failure
    Parser inner  

let ( >>= ) = bindParser


let mapParser parser func =
    let inner stream =
        let result = runParser parser stream
        match result with 
        | Success (res, rest) -> Success(func res, rest)
        | Failure -> Failure
    Parser inner

let ( |>> ) = mapParser

let rec many parser =
    (parser >>= fun res -> many parser |>> fun r -> res::r)
    <|> returnParser []
        
   
let charParser char =
    let inner stream =
        match stream with
        | x::xs when char = x -> Success(char, xs)
        | _ -> Failure
    Parser inner

  
let myParser = many (charParser 'a') .>>. charParser 'b'

let result = runParser myParser (List.ofSeq "aaaab")



