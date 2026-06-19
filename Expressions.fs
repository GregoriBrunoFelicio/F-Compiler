module Expressions

type Expression =
    | BindingExpression of obj
    | IdentifierExpression of string
    // | Binary of Expression * TokenType * Expression
    | LetExpression of string * Expression
    | PrintExpression of Expression
// | If of Expression * Expression list * Expression list option
// | For of string * Expression * Expression list
