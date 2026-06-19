module Emitter

open System
open System.Reflection.Emit
open Expressions
open EmitContext

let emitLiteral (il: ILGenerator, value: obj) =
    match value with
    | :? int as value ->
        il.Emit(OpCodes.Ldc_I4, value)
        typeof<int>
    | :? string as value ->
        il.Emit(OpCodes.Ldstr, value)
        typeof<string>
    | _ -> failwith "Unsuported literal"

let bindingEmitter (name: string, il: ILGenerator, context: EmitContext) =
    match context.Locals.TryFind name with
    | Some local ->
        il.Emit(OpCodes.Ldloc, local)
        local.LocalType
    | None -> failwith $"Variable '{name}' not found"

let emitExpression (il: ILGenerator, expression: Expression, context: EmitContext) =
    match expression with
    | LiteralExpression(obj: obj) -> emitLiteral (il, obj)
    | IdentifierExpression(name: string) -> bindingEmitter (name, il, context)
    | _ -> failwith "Unsuported expression"

let printEmitter (il: ILGenerator, expression: Expression, context: EmitContext) =
    let clrType = emitExpression (il, expression, context)
    let writeLine = typeof<Console>.GetMethod("WriteLine", [| clrType |])
    il.Emit(OpCodes.Call, writeLine)
    context

let letEmitter (name: string, il: ILGenerator, expression: Expression, context: EmitContext) =
    let clrType = emitExpression (il, expression, context)
    let local = il.DeclareLocal clrType
    il.Emit(OpCodes.Stloc, local)

    let newContext =
        { context with
            Locals = context.Locals.Add(name, local) }

    newContext
