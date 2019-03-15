// Learn more about F# at http://fsharp.org
open Microsoft.FSharp.Text.Lexing
open AST
open System

let mutable stack: List<Map<string, Atomic>> = Map.empty :: List.empty

let read (id: string) = 
  List.tryPick (fun scope -> if Map.containsKey id scope then scope.[id] |> Some else None) stack
  |> fun v -> v.Value

let add (id: string) (v: Atomic) =
  let scope = stack.Head |> fun scope -> scope.Add (id, v)
  stack <- scope :: stack.Tail
  ()

let write (id: string) (v: Atomic) =
  let scopeIndex = List.tryFindIndex (Map.containsKey id) stack
  let scope = stack.[scopeIndex.Value] |> fun scope -> scope.Add (id, v)
  stack <- List.mapi (fun i v -> if i = scopeIndex.Value then scope else v) stack
  ()

let rec evalExpr (expr: Expression) =
  match expr with
  | Equals (l, r)      -> (evalExpr l, evalExpr r) ||> fun (l: Atomic) r -> l.IsEqual r |> Bool
  | NotEquals (l, r)   -> (evalExpr l, evalExpr r) ||> fun (l: Atomic)  r -> l.IsNotEqual r |> Bool
  | Bigger (l, r)      -> (evalExpr l, evalExpr r) ||> fun (l: Atomic)  r -> l.IsBigger r |> Bool
  | Smaller (l, r)     -> (evalExpr l, evalExpr r) ||> fun (l: Atomic)  r -> l.IsSmaller r |> Bool
  | Atomic a           -> a
  | Id id              -> read id
  | Not e              -> evalExpr e |> fun r -> match r with
                                                  | Bool b -> not b |> Bool
                                                  | _      -> raise <| Exception "exepected Bool expr for '!'"
  | Nested e           -> evalExpr e

and evalCodeBlock (cb: CodeBlock) = 
  stack <- Map.empty :: stack
  match cb with
  | CodeBlock cb -> List.map evalStm cb |> ignore
  stack <- stack.Tail

and evalStm (stm: Statement) = 
  match stm with
  | Declaration (name, expr) -> add name <| evalExpr expr
  | Assigment (name, expr)   -> write name <| evalExpr expr
  | If (pred, _then, _else)  -> evalExpr pred |> fun c -> if c = Bool true
                                                           then evalCodeBlock _then |> ignore
                                                           elif _else.IsSome
                                                           then evalCodeBlock _else.Value |> ignore
  | Echo e                   -> evalExpr e |> printf "%A\n"
  | Expression e             -> evalExpr e |> ignore

[<EntryPoint>]
let main argv =

  let test = """
    let x = 2
    let y = 1
    
    if true == x < 2 == false then {
      let x = 5
      y = 5
      echo x
    }

    echo x
  """

  let lexbuf = LexBuffer<char>.FromString test
  let ast: Statement list = Parser.start Lexer.tokenstream lexbuf

  printf "%A \n\n" ast

  List.map evalStm ast |> ignore


  0 