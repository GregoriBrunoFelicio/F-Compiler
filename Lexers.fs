module Lexers

open Helpers
open Token
open System

let identifierLexer text current =
    let value, position = readWhile text Char.IsLetterOrDigit current
    let tokenType = getToken value
    let token = { Type = tokenType; Value = value }
    token, position

let numberLexer text current =
    let value, position = readWhile text Char.IsDigit current
    let token = { Type = Number; Value = value }
    token, position

let symbolLexer text current =
    let tokenType = getSymbolToken text

    let token =
        { Type = tokenType
          Value = string text }

    token, current + 1

let stringLexer text current =
    let value, position = readWhile text (fun c -> c <> '"') (current + 1)
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
                let token, nextIndex = symbolLexer current index
                loop nextIndex (token :: tokens)

            else
                failwithf "Unexpected: %c" current
        else
            let endOfLineToken = { Type = EndOfFile; Value = "" }
            endOfLineToken :: tokens |> List.rev

    loop 0 []
