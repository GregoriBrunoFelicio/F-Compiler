open Lexers
open Parser

let code = "let num = 10;"
let tokens = tokenize code

let b = parser tokens
