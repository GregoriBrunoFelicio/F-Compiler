open Token
open Expressions
open Lexers

let code = "let num = 10;"
let tokens = tokenize code

for t in tokens do
    printf "%A\n" t


