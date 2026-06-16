module Token

type TokenType =
    | SemiColon
    | Let
    | Identifier
    | Equals
    | EndOfFile
    | Number
    | String
    | Print
    | LeftParen
    | RightParen
    | Plus

type Token = { Type: TokenType; Value: string }

let getToken text =
    match text with
    | "let" -> Let
    | "print" -> Print
    | _ -> Identifier

let getSymbolToken text =
    match text with
    | ';' -> SemiColon
    | '=' -> Equals
    | '(' -> LeftParen
    | ')' -> RightParen
    | '+' -> Plus
    | _ -> Identifier

let isLiteralToken tokenType =
    match tokenType with
    | Number
    | String -> true
    | _ -> false
