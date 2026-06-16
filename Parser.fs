module Parser

open Token
open Expressions


type ParserState = { Tokens: list<Token>; Position: int }

let current state = state.Tokens[state.Position]

let advance state =
    { state with
        Position = state.Position + 1 }

let expect expectedType state =
    if (current state).Type = expectedType then
        advance state
    else
        failwithf "Expected %A but got %A" expectedType (current state).Type

let letParser state =

    let state = expect Let state

    let name = (current state).Value
    let state = advance state

    let state = expect Equals state

    // TODO: check if it is really a literal expression
    let value = (current state).Value
    let state = advance state

    let state = expect SemiColon state

    LetExpression(name, LiteralExpression value), state


let parser (tokens: list<Token>) =
    let rec loop expressions state =
        if state.Position > tokens.Length || (current state).Type = EndOfFile then
            expressions
        else if (current state).Type = TokenType.Let then
            let exp, nextState = letParser state
            loop (exp :: expressions) nextState
        else
            printfn "Unexpected token: %A" (current state)
            []

    let initialState = { Tokens = tokens; Position = 0 }
    loop [] initialState
