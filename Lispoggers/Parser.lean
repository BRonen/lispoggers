import Lispoggers.Lexer

inductive CST where
  | identifier : Location → String → CST
  | list       : Location → List CST → CST
  | map        : Location → List CST → CST
  | error      : Location → String → CST
  deriving Repr

inductive FrameKind | List | Map

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
    | _, _ => [⟨[CST.error ⟨0, 0, 0⟩ "Parse error"], ⟨0, 0, 0⟩, FrameKind.List⟩]

  let initial := [⟨[], ⟨0, 0, 0⟩, FrameKind.List⟩]
  match tokens.foldl step initial with
  | [frame] =>
    match frame.elements.reverse with
    | [cst] => cst
    | [] => CST.error ⟨0, 0, 0⟩ "Empty input"
    | _ => CST.error ⟨0, 0, 0⟩ "Multiple top-level expressions"
  | _ => CST.error ⟨0, 0, 0⟩ "Unmatched brackets"

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
    if elements.length % 2 == 0
    then AST.map loc (parseElements elements)
    else AST.error loc "Map must have even number of elements"

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
