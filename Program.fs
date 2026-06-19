open Lexers
open Parser
open CodeGen

let code =
    @"num -> ""dasd""
      println num"

let tokens = tokenize code

// for ex in tokens do
//     printf "%A\n" ex
//
let expressions = parser tokens

for ex in expressions do
    printf "%A\n" ex


compileAndRun expressions |> ignore
