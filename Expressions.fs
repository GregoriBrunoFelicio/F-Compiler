module Expressions

open Token

type Expression =
    | LiteralExpression of obj
    | IdentifierExpression of string
    | BinaryExpression of Expression * TokenType * Expression
    | BindingExpression of string * Expression
    | PrintExpression of Expression
// | If of Expression * Expression list * Expression list option
// | For of string * Expression * Expression list
