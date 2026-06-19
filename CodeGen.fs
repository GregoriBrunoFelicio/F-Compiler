module CodeGen

open System.Reflection
open System.Reflection.Emit
open System
open Expressions
open Emitter

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

    // EXPRESSIONS
    for ex in expressions do
        match ex with
        | LetExpression(str, ex) -> letEmitter (il, ex)
        | PrintExpression ex -> printEmitter (il, ex)
        | _ -> () // Maybe ret here?

    // END

    // il.Emit OpCodes.Pop
    il.Emit OpCodes.Ret
    let programType = typeBuilder.CreateType()

    let main =
        programType.GetMethod("Main", BindingFlags.Public ||| BindingFlags.Static)

    main.Invoke(null, null)
