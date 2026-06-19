module Token

type TokenType =
    | SemiColon
    | Binding
    | Identifier
    | Equals
    | Number
    | String
    | PrintLn
    | LeftParen
    | RightParen
    | Plus
    | EndOfFile
    | NewLine
    | BlockOpen
    | BlockClose
    | Minus
    | GreaterThan
    | GreaterThanOrEqual
    | LessThan
    | LessThanOrEqual
    | EqualsEquals

type Token = { Type: TokenType; Value: string }

let getToken text =
    match text with
    | "->" -> Binding
    | "println" -> PrintLn
    | _ -> Identifier

let isLiteralToken tokenType =
    match tokenType with
    | Number
    | String -> true
    | _ -> false

let getSymbolToken text index =
    if index + 1 < String.length text then
        match text[index], text[index + 1] with
        | '-', '>' -> Some(Binding, "->", 2)
        | '=', '=' -> Some(EqualsEquals, "==", 2)
        | '<', '=' -> Some(LessThanOrEqual, "<=", 2)
        | '>', '=' -> Some(GreaterThanOrEqual, ">=", 2)
        | _ ->
            match text[index] with
            | ';' -> Some(SemiColon, ";", 1)
            | '=' -> Some(Equals, "=", 1)
            | '(' -> Some(LeftParen, "(", 1)
            | ')' -> Some(RightParen, ")", 1)
            | '+' -> Some(Plus, "+", 1)
            | '-' -> Some(Minus, "-", 1)
            | '>' -> Some(GreaterThan, ">", 1)
            | '<' -> Some(LessThan, "<", 1)
            | _ -> None
    else
        match text[index] with
        | ';' -> Some(SemiColon, ";", 1)
        | '=' -> Some(Equals, "=", 1)
        | '(' -> Some(LeftParen, "(", 1)
        | ')' -> Some(RightParen, ")", 1)
        | '+' -> Some(Plus, "+", 1)
        | '-' -> Some(Minus, "-", 1)
        | '>' -> Some(GreaterThan, ">", 1)
        | '<' -> Some(LessThan, "<", 1)
        | _ -> None
