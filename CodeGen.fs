module CodeGen

open System.Reflection
open System.Reflection.Emit
open System
open Expressions
open Emitter
open EmitContext

let createBuilders =
    let assemblyName = AssemblyName "MyProgram"

    let assemblyBuilder =
        AssemblyBuilder.DefineDynamicAssembly(assemblyName, AssemblyBuilderAccess.Run)

    let moduleBuilder = assemblyBuilder.DefineDynamicModule "MainModule"
    let typeBuilder = moduleBuilder.DefineType("Program", TypeAttributes.Public)

    let methodBuilder =
        typeBuilder.DefineMethod("Main", MethodAttributes.Public ||| MethodAttributes.Static, typeof<Void>, [||])

    let il = methodBuilder.GetILGenerator()
    il, typeBuilder

let compileAndRun (expressions: list<Expression>) =
    let il, typeBuilder = createBuilders

    // EXPRESSIONS START
    let rec loop (expressions: list<Expression>, context: EmitContext) =
        match expressions with
        | head :: tail ->
            let newContext =
                match head with
                | LetExpression(name, ex) -> letEmitter (name, il, ex, context)
                | PrintExpression ex -> printEmitter (il, ex, context)
                | _ -> failwith $"Unsuported expression: {head}"

            loop (tail, newContext)

        | [] -> context

    let initialContext = { Locals = Map.empty }
    loop (expressions, initialContext) |> ignore
    // EXPRESSIONS END

    il.Emit OpCodes.Ret
    let programType = typeBuilder.CreateType()

    let main =
        programType.GetMethod("Main", BindingFlags.Public ||| BindingFlags.Static)

    main.Invoke(null, null)
