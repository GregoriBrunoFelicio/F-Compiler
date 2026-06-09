module Expressions

type Expression =
    | Literal of obj

type LetExpression = 
    { 
      BindingName : string
      Value : Expression 
    }


