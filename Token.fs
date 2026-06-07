module Token

type TokenType =
    | SemiColon
    | Let
    | Identifier
    | Equals
    | EndOfFile
    | Number
    | String

type Token = 
    { Type : TokenType
      Value : string 
    }

let getIdentifierToken text = 
    match text with
    | "let" -> Let
    | _ -> Identifier
    

let getSymbolToken text = 
    match text with
    | ';' -> SemiColon
    | '=' -> Equals
    | _ -> Identifier
    

