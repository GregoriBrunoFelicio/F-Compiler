module Parser

open Token
open Expressions


type ParserState = { Tokens: list<Token>; Position: int }

let currentToken state = state.Tokens[state.Position]

let advance state =
    { state with
        Position = state.Position + 1 }

let expect expectedType state =
    if (currentToken state).Type = expectedType then
        advance state
    else
        failwithf "Expected %A but got %A" expectedType (currentToken state).Type

let parserLiteral state =
    let token = currentToken state

    match token.Type with
    | Number -> LiteralExpression token.Value // TODO: Parse to number
    | String -> LiteralExpression token.Value
    | tokenType -> failwithf "Expected literal but got %A" tokenType

let expressionParser state =
    let token = currentToken state

    if isLiteralToken token.Type then
        advance state, parserLiteral state
    else if token.Type = Identifier then
        advance state, IdentifierExpression token.Value
    else
        failwithf "Unexpected: %A" token.Value

let letParser state =
    let state = expect Let state
    let name = (currentToken state).Value
    let state = advance state
    let state = expect Equals state
    let state, expression = expressionParser state
    let state = expect SemiColon state
    LetExpression(name, expression), state

let printParser state =
    let state = expect Print state
    let state, expression = expressionParser state
    PrintExpression expression, state


let parser (tokens: list<Token>) =
    let rec loop expressions state =
        if state.Position > tokens.Length || (currentToken state).Type = EndOfFile then
            expressions
        else if (currentToken state).Type = Let then
            let exp, nextState = letParser state
            loop (exp :: expressions) nextState
        else if (currentToken state).Type = Print then
            let exp, nextState = printParser state
            loop (exp :: expressions) nextState
        else
            printfn "Unexpected token: %A" (currentToken state)
            []

    let initialState = { Tokens = tokens; Position = 0 }
    loop [] initialState
