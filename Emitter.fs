module Emitter

open System
open System.Reflection.Emit
open Expressions

type CompilerContext =
    { Variables: Map<string, LocalBuilder> }


let emitLiteral (il: ILGenerator, value: obj) =
    match value with
    | :? int as value -> il.Emit(OpCodes.Ldc_I4, value)
    | :? string as value -> il.Emit(OpCodes.Ldstr, value)
    | _ -> failwith "Unsuported literal"

let emitToStack (il: ILGenerator, expression: Expression) =
    match expression with
    | LiteralExpression(_, obj) -> emitLiteral (il, obj)
    | _ -> failwith "Unsuported expression"

let printEmitter (il: ILGenerator, expression: Expression) =
    emitToStack (il, expression)
    let writeLine = typeof<Console>.GetMethod("WriteLine", [| typeof<int> |])
    il.Emit(OpCodes.Call, writeLine)


let letEmitter (il: ILGenerator, expression: Expression) = emitToStack (il, expression)
