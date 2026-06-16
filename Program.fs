open Lexers
open Parser

let code = "let num = 10;"
let tokens = tokenize code

let expressions = parser tokens

for ex in expressions do
    printf "%A\n" ex
