module Parser

open Token
open Expressions


let letParser (tokens: list<Token>, index) =
    let expression = Let("let", Literal 10)
    expression, index


let parser (tokens: list<Token>) =
    let rec loop expressions index =
        let current = tokens[index]

        if index > tokens.Length then
            expressions

        else if current.Type = TokenType.Let then
            let exp, i = letParser (tokens, index)
            printfn "%A" exp
            loop (exp :: expressions) (i + 1)

        // else if current.Type = TokenType.Identifier then
        //     let ex, i = identifierParser (tokens, index)
        //     printfn "%A" ex
        //     loop (ex :: expressions) (i + 1)
        //
        else
            printfn "Unexpected token: %A" current
            []

    loop [] 0
