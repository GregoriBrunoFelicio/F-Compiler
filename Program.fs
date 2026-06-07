open Token
open System
open Helpers
open Lexers


let code = "let name = \"Greg Felicio\";"

let tokenize text =
    let size = String.length text - 1
    let rec loop index tokens =
        if index <= size then
            let current = text[index]

            if Char.IsWhiteSpace current then
                loop (index+1) tokens

            else if Char.IsLetter current then
                let token, nextIndex = identifierLexer text index
                loop nextIndex (token::tokens)

            else if current = '"' then
                let token, nextIndex = stringLexer text index
                loop nextIndex (token::tokens)

            else if Char.IsDigit current then
                let token, nextIndex = numberLexer text index
                loop nextIndex (token::tokens)

            else if isSymbol current then
                let token, nextIndex = symbolLexer current index
                loop nextIndex (token::tokens)
            else
                failwithf "Unexpected: %c" current
        else
            let endOfLineToken = 
                {
                    Type = EndOfFile
                    Value = ""
                }
            endOfLineToken::tokens |> Seq.rev

    loop 0 []        

        

let tokens = tokenize code

for t in tokens do
    printf "%A\n" t

