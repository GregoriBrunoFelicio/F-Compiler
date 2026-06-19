module EmitContext

open System.Reflection.Emit

type EmitContext = { Locals: Map<string, LocalBuilder> }
