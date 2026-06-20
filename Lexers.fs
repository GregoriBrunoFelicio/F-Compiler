module Lexers

open Helpers
open Token
open System

let identifierLexer text index =
    let value, position = readWhile text Char.IsLetterOrDigit index

    let tokenType =
        match keyWordsTokenMap.TryFind value with
        | Some value -> value
        | None -> Identifier

    let token = { Type = tokenType; Value = value }
    token, position

let numberLexer text index =
    let value, position = readWhile text Char.IsDigit index
    let token = { Type = Number; Value = value }
    token, position

let symbolLexer text index =
    match getSymbolToken text index with
    | Some(tokenType, value, offset) -> { Type = tokenType; Value = value }, index + offset
    | None -> failwithf $"Unknown symbol: %c{text[index]}"

let stringLexer text index =
    let value, position = readWhile text (fun c -> c <> '"') (index + 1)
    let token = { Type = Token.String; Value = value }
    token, position + 1

let tokenize text =
    let size = String.length text - 1

    let rec loop index tokens =
        if index > size then
            { Type = EndOfFile; Value = "" } :: tokens |> List.rev
        else
            let current = text[index]

            let continueWith lexer =
                let token, nextIndex = lexer text index
                loop nextIndex (token :: tokens)

            match current with
            | c when Char.IsWhiteSpace c -> loop (index + 1) tokens
            | c when Char.IsLetter c -> continueWith identifierLexer
            | c when isOnlyQuotes c -> continueWith stringLexer
            | c when Char.IsDigit c -> continueWith numberLexer
            | c when isSymbol c -> continueWith symbolLexer
            | c -> failwithf $"Unexpected character: %c{c}"

    loop 0 []
