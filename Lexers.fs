module Lexers

open Helpers
open Token
open System

let identifierLexer text index =
    let value, position = readWhile text Char.IsLetterOrDigit index
    let tokenType = getToken value
    let token = { Type = tokenType; Value = value }
    token, position

let numberLexer text index =
    let value, position = readWhile text Char.IsDigit index
    let token = { Type = Number; Value = value }
    token, position

let symbolLexer text index =
    match getSymbolToken text index with
    | Some(tokenType, value, offset) -> { Type = tokenType; Value = value }, index + offset
    | None -> failwithf "Unknown symbol: %c" text[index]

let stringLexer text index =
    let value, position = readWhile text (fun c -> c <> '"') (index + 1)
    let token = { Type = Token.String; Value = value }
    token, position + 1


let tokenize text =
    let size = String.length text - 1

    let rec loop index tokens =
        if index <= size then
            let current = text[index]

            if Char.IsWhiteSpace current then
                loop (index + 1) tokens

            else if Char.IsLetter current then
                let token, nextIndex = identifierLexer text index
                loop nextIndex (token :: tokens)

            else if isOnlyQuotes current then
                let token, nextIndex = stringLexer text index
                loop nextIndex (token :: tokens)

            else if Char.IsDigit current then
                let token, nextIndex = numberLexer text index
                loop nextIndex (token :: tokens)

            else if isSymbol current then
                let token, nextIndex = symbolLexer text index
                loop nextIndex (token :: tokens)

            else
                failwithf "Unexpected: %c" current
        else
            let endOfLineToken = { Type = EndOfFile; Value = "" }
            endOfLineToken :: tokens |> List.rev

    loop 0 []
