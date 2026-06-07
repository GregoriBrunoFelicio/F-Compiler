
module Lexers

open Helpers
open Token
open System

let identifierLexer text current =
    let value, position =
        readWhile text (fun c -> Char.IsLetterOrDigit c || c = '_') current
    let tokenType = getIdentifierToken value
    let token =
         {
            Type = tokenType
            Value = value
         }
    token, position

let numberLexer text current =
    let value, position = readWhile text Char.IsDigit current
    let token =
         {
            Type = Number
            Value = value
         }
    token, position

let symbolLexer text current =
    let tokenType = getSymbolToken text
    let token =
         {
            Type = tokenType
            Value = string text
         }
    token, current+1

let stringLexer text current = 
     let value, position =
        readWhile text (fun c -> c <> '"') (current+1)
     let token =
         {
            Type = Token.String
            Value = value
         }
     token, position+1
      



     




















