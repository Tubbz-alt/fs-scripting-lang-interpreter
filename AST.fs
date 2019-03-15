module AST
open System

type Expression =
  | Equals of left: Expression * right: Expression
  | NotEquals of left: Expression * right: Expression
  | Not of Expression
  | Bigger of left: Expression * right: Expression
  | Smaller of left: Expression * right: Expression
  | Atomic of Atomic
  | Nested of Expression
  | Id of string
  | And of left: Expression * right: Expression
  | Or of left: Expression * right: Expression

and Atomic = 
  | String of string
  | Int of int
  | Bool of bool
with member l.IsEqual (r: Atomic) =
        match (l, r) with
        | (Bool l, Bool r)      -> l = r
        | (Int l, Int r)        -> l = r
        | (String l, String r)  -> l = r
        | _                     -> raise <| Exception "exepected Bool expr for '=='"
      
      member l.IsNotEqual (r: Atomic) =
        match (l, r) with
        | (Bool l, Bool r)      -> l <> r
        | (Int l, Int r)        -> l <> r
        | (String l, String r)  -> l <> r
        | _                     -> raise <| Exception "exepected Bool expr for '!='"

      member l.IsBigger (r: Atomic) =
        match (l, r) with
        | (Int l, Int r)        -> l > r
        | _                     -> raise <| Exception "exepected Int expr for '>'"

      member l.IsSmaller (r: Atomic) =
        match (l, r) with
        | (Int l, Int r)        -> l < r
        | _                     -> raise <| Exception "exepected Int expr for '<'"


and Statement = 
  | Declaration of name: string * expr: Expression
  | Assigment of name: string * expr: Expression
  | If of predicate: Expression * _then: CodeBlock * _else: CodeBlock option
  | Echo of Expression
  | Expression of Expression

and CodeBlock = CodeBlock of Statement list
