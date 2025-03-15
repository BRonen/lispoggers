import Batteries
import Lispoggers.Lexer

inductive CST where
  | identifier : Location → String → CST
  | list       : Location → List CST → CST
  | map        : Location → List CST → CST
  | error      : Location → String → CST
  deriving Repr

inductive FrameKind
  | List
  | Map

structure Frame where
  elements : List CST
  loc : Location
  kind : FrameKind

def concreteParse (tokens : List Token) : CST :=
  let rec step (stack : List Frame) (token : Token) : List Frame :=
    match token, stack with
    | Token.identifier loc str, frame :: rest =>
      { frame with elements := CST.identifier loc str :: frame.elements } :: rest
    | Token.lparen loc, stack =>
      ⟨[], loc, FrameKind.List⟩ :: stack
    | Token.lcurly loc, stack =>
      ⟨[], loc, FrameKind.Map⟩ :: stack
    | Token.rparen _, frame :: parent :: rest =>
      let cst := match frame.kind with
        | FrameKind.List => CST.list frame.loc (frame.elements.reverse)
        | FrameKind.Map => CST.error frame.loc "Mismatched brackets"
      { parent with elements := cst :: parent.elements } :: rest
    | Token.rcurly _, frame :: parent :: rest =>
      let cst := match frame.kind with
        | FrameKind.Map => CST.map frame.loc (frame.elements.reverse)
        | FrameKind.List => CST.error frame.loc "Mismatched brackets"
      { parent with elements := cst :: parent.elements } :: rest
    | Token.error loc msg, frame :: rest =>
      { frame with elements := CST.error loc msg :: frame.elements } :: rest
    | _, _ => [⟨[CST.error ⟨0, 0, 0, ""⟩ "Parse error"], ⟨0, 0, 0, ""⟩, FrameKind.List⟩]

  let initial := [⟨[], ⟨0, 0, 0, ""⟩, FrameKind.List⟩]
  match tokens.foldl step initial with
  | [frame] =>
    match frame.elements.reverse with
    | [cst] => cst
    | [] => CST.error ⟨0, 0, 0, ""⟩ "Empty input"
    | _ => CST.error ⟨0, 0, 0, ""⟩ "Multiple top-level expressions"
  | _ => CST.error ⟨0, 0, 0, ""⟩ "Unmatched brackets"

#eval concreteParse (lexer "(a b c)")
#eval concreteParse (lexer "{a b c}")
#eval concreteParse (lexer "{{{{a (b c) d}} e}}")
#eval concreteParse (lexer "(a (b c) {d e})")

inductive AST where
  | list       : Location → List AST → AST
  | map        : Location → List AST → AST
  | lambda     : Location → AST → AST → AST → AST → AST
  | identifier : Location → String → AST
  | error      : Location → String → AST
  deriving Repr

mutual
  def parseElements : List CST → List AST
    | [] => []
    | x :: xs => parseNode x :: parseElements xs

  def parseList (loc : Location) : List CST → AST
    | [] => AST.list loc []
    | CST.identifier _ "lambda" :: param :: paramType :: returnType :: body :: [] =>
      AST.lambda loc (parseNode param) (parseNode paramType) (parseNode returnType) (parseNode body)
    | elements => AST.list loc (parseElements elements)

  def parseMap (loc : Location) (elements : List CST) : AST :=
    AST.map loc (parseElements elements)

  def parseNode : CST → AST
    | CST.identifier loc str => AST.identifier loc str
    | CST.list loc elements => parseList loc elements
    | CST.map loc elements => parseMap loc elements
    | CST.error loc msg => AST.error loc msg
end

def abstractParse : CST → AST := parseNode

#eval abstractParse (concreteParse (lexer "(lambda x Bool Bool x)"))
#eval abstractParse (concreteParse (lexer "{a b c d}"))
#eval abstractParse (concreteParse (lexer "{a b c}"))
#eval abstractParse (concreteParse (lexer "(f x y)"))

inductive SyntaxTree where
  | list       : Location → List SyntaxTree → SyntaxTree
  | map        : Location → List SyntaxTree → SyntaxTree
  | lambda     : Location → SyntaxTree → SyntaxTree → SyntaxTree → SyntaxTree
  | identifier : Location → Nat → SyntaxTree
  | error      : Location → String → SyntaxTree
  deriving Repr, Nonempty

partial def parse (ctx : Batteries.RBMap String Nat compare) (acc : Nat) : AST → SyntaxTree
  | AST.list loc elements => SyntaxTree.list loc (elements.map $ parse ctx acc)
  | AST.map loc elements => SyntaxTree.map loc (elements.map $ parse ctx acc)
  | AST.lambda loc name t r b =>
    let acc := acc + 1
    let t := parse ctx acc t
    let r := parse ctx acc r
    match name with
    | AST.identifier loc name => 
      let ctx := ctx.insert name acc
      let b := parse ctx acc b
      SyntaxTree.lambda loc t r b
    | _ => SyntaxTree.error loc "wasdwasd"
  | AST.identifier loc name =>
    match ctx.find? name with
    | some i => SyntaxTree.identifier loc i
    | none => SyntaxTree.error loc "Identifier not defined"
  | AST.error loc err => SyntaxTree.error loc err

def dctx : Batteries.RBMap String Nat compare := Batteries.RBMap.empty.insert "Type" 0

#eval "(lambda Bool Type Type (lambda x Bool Bool x))" |> lexer |> concreteParse |> abstractParse |> parse dctx 0
#eval "{a b c d}" |> lexer |> concreteParse |> abstractParse |> parse Batteries.RBMap.empty 0
#eval "{a b c}" |> lexer |> concreteParse |> abstractParse |> parse Batteries.RBMap.empty 0
#eval "(a b c d)" |> lexer |> concreteParse |> abstractParse |> parse Batteries.RBMap.empty 0
#eval "(a b c)" |> lexer |> concreteParse |> abstractParse |> parse Batteries.RBMap.empty 0
#eval "((lambda x Type Type (x x)) (lambda x Type Type (x x)))" |> lexer |> concreteParse |> abstractParse |> parse dctx 0

