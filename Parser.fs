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
    | Number -> BindingExpression(System.Convert.ToInt32 token.Value) // TODO: Parse to number
    | String -> BindingExpression token.Value
    | tokenType -> failwithf "Expected literal but got %A" tokenType

let expressionParser state =
    let token = currentToken state

    if isLiteralToken token.Type then
        advance state, parserLiteral state
    else if token.Type = Identifier then
        advance state, IdentifierExpression token.Value
    else
        failwithf "Unexpected: %A" token.Value

let bindingParser state =
    let name = (currentToken state).Value
    let state = expect Identifier state
    let state = expect Binding state
    let state, expression = expressionParser state
    LetExpression(name, expression), state

let printParser state =
    let state = expect PrintLn state
    let state, expression = expressionParser state
    PrintExpression expression, state

let parser (tokens: list<Token>) =
    let rec loop expressions state =
        if state.Position > tokens.Length || (currentToken state).Type = EndOfFile then
            expressions |> List.rev
        else if (currentToken state).Type = Identifier then
            // Future problem: it could be a function
            let exp, nextState = bindingParser state
            loop (exp :: expressions) nextState
        else if (currentToken state).Type = PrintLn then
            let exp, nextState = printParser state
            loop (exp :: expressions) nextState
        else
            printfn "Unexpected token: %A" (currentToken state)
            []

    let initialState = { Tokens = tokens; Position = 0 }
    loop [] initialState
