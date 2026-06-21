open Lexers
open Parser
open CodeGen

let code =
    @"foo -> true
      println foo"

let tokens = tokenize code

// for ex in tokens do
//     printf "%A\n" ex
//
let expressions = parser tokens

// f:or ex in expressions do
//     printf "%A\n" ex
//
compileAndRun expressions |> ignore
