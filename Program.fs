open Lexers
open Parser
open CodeGen
open System.Reflection.Emit
open System
open Expressions

let code =
    "let num = 10;
     print num"

let tokens = tokenize code
let expressions = parser tokens

for ex in expressions do
    printf "%A\n" ex

compileAndRun expressions |> ignore
