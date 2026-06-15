module Expressions

// type Expression = Literal of obj
//
// type LetExpression =
//     { BindingName: string
//       Value: Expression }
//
//
// type IdentifierExpression = { Name: string }

type Expression =
    | Literal of obj
    | Identifier of string
    // | Binary of Expression * TokenType * Expression
    | Let of string * Expression
// | Print of Expression
// | If of Expression * Expression list * Expression list option
// | For of string * Expression * Expression list
