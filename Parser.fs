module Parser

open Token
open Expressions
open Helpers

type ParserState = { Tokens: list<Token>; Position: int }

let currentToken state = state.Tokens[state.Position]

let advance state =
    { state with
        Position = state.Position + 1 }

let expect expectedType state =
    if (currentToken state).Type = expectedType then
        advance state
    else
        failwithf $"Expected %A{expectedType} but got %A{(currentToken state).Type}"

let literalParser state =
    let token = currentToken state

    match token.Type with
    | Number -> LiteralExpression(System.Convert.ToInt32 token.Value) // TODO: Parse to number
    | String -> LiteralExpression token.Value
    | BooleanTrue -> LiteralExpression true
    | BooleanFalse -> LiteralExpression false
    | tokenType -> failwithf $"Expected literal but got %A{tokenType}"

let expressionParser state =
    let token = currentToken state

    if isLiteralToken token.Type then
        advance state, literalParser state
    else if token.Type = Identifier then
        advance state, IdentifierExpression token.Value
    else
        failwithf $"Unexpected: %A{token.Value}"


let blalbla state =
    let (state, left) = expressionParser state
    let current = currentToken state
    // Contains operatins e.g +
    if current.Type = Plus then
        let state = advance state
        let (state, right) = expressionParser state
        state, BinaryExpression(left, Plus, right)
    else
        state, left

let bindingParser state =
    let token = currentToken state
    let name = token.Value
    ensureValidIdentifier name
    let state = expect Identifier state
    let state = expect Binding state
    let state, expression = blalbla state
    BindingExpression(name, expression), state

let printParser state =
    let state = expect PrintLn state
    let state, expression = blalbla state
    PrintExpression expression, state

let isBinding state =
    match List.tryItem (state.Position + 1) state.Tokens with
    | Some { Type = Binding } -> true
    | _ -> false

let parser (tokens: list<Token>) =
    let rec loop expressions state =
        match currentToken state with
        | { Type = EndOfFile } -> List.rev expressions

        | { Type = PrintLn } ->
            let exp, nextState = printParser state
            loop (exp :: expressions) nextState

        | _ when isBinding state ->
            let exp, nextState = bindingParser state
            loop (exp :: expressions) nextState

        | token -> failwithf $"Unexpected token: %A{token}"

    let initialState = { Tokens = tokens; Position = 0 }

    loop [] initialState
