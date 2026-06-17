open Lexers
open Parser
open System.Reflection.Emit
open System
open Expressions

let code =
    "let num = 1234124312;
     print num"

let tokens = tokenize code
//
// for t in tokens do
//     printfn "%A" t
//
let expressions = parser tokens

// for ex in expressions do
//     printf "%A\n" ex
//
let codeGen (expressions: list<Expression>) =

    let method = DynamicMethod("main", typeof<Void>, Type.EmptyTypes)
    let il = method.GetILGenerator()

    for expression in expressions do
        match expression with
        | LetExpression(str, ex) ->
            let local = il.DeclareLocal typeof<int> // TODO

            match ex with
            | LiteralExpression value ->
                let valueInt = System.Convert.ToInt32 value // TODO identify type
                il.Emit(OpCodes.Ldc_I4, valueInt)
                il.Emit(OpCodes.Stloc, local)
            | _ -> ()

            il.Emit(OpCodes.Ldloc, local)

            let writeLine = typeof<Console>.GetMethod("WriteLine", [| typeof<int> |])

            il.Emit(OpCodes.Call, writeLine)

            il.Emit OpCodes.Ret

            let action = method.CreateDelegate typeof<Action> :?> Action

            action.Invoke()
        | _ -> ()



codeGen expressions
