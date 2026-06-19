module Helpers

open System

let isSymbol text =
    match text with
    | ';' -> true
    | '=' -> true
    | '(' -> true
    | ')' -> true
    | '+' -> true
    | '-' -> true
    | '>' -> true
    | '<' -> true
    | _ -> false

let isEndOfLine text = text = ';'

let isOnlyQuotes text = text = '"'

let readWhile texts fn current =
    let array = texts |> Seq.toArray

    let rec loop aux current =
        if current >= Array.length array then
            aux, current
        else
            let c = array[current]

            if fn c then loop (c :: aux) (current + 1) else aux, current

    let chars, current = loop [] current
    let text = chars |> List.rev |> List.toArray |> String
    text, current
