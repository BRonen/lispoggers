import Lispoggers.Lexer

namespace Lispoggers.Parser

open Lispoggers.Lexer

inductive CST where
  | string     : Location → String → CST
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

#eval concreteParse (Lexer.lexer "(a b c)")
#eval concreteParse (Lexer.lexer "{a b c}")
#eval concreteParse (Lexer.lexer "{{{{a (b c) d}} e}}")
#eval concreteParse (Lexer.lexer "(a (b c) {d e})")

inductive AST where
  | string     : Location → String → AST
  | list       : Location → List AST → AST
  | map        : Location → List AST → AST
  | definition : Location → (label : AST) → (value : AST) → AST
  | apply      : Location → (callee : AST) → (arg : List AST) → AST
  | lambda     : Location → (param : AST) → (paramType : AST) → (returnType : AST) → (body : AST) → AST
  | macro      : Location → (param : AST) → (paramType : AST) → (returnType : AST) → (body : AST) → AST
  | data       : Location → (deps : AST) → (constructors : List AST) → AST
  | identifier : Location → String → AST
  | error      : Location → String → AST
  deriving Repr

mutual
  def parseElements : List CST → List AST
    | [] => []
    | x :: xs => parseNode x :: parseElements xs

  def parseList (loc : Location) : List CST → AST
    | [] => AST.list loc []
    | CST.identifier _ "list" :: elements =>
      AST.list loc (parseElements elements)
    | CST.identifier _ "def" :: label :: element :: [] =>
      AST.definition loc (parseNode label) (parseNode element)
    | CST.identifier _ "data" :: deps :: constructors =>
      AST.data loc (parseNode deps) (parseElements constructors)
    | CST.identifier _ "macro" :: param :: paramType :: returnType :: body :: [] =>
      AST.macro loc (parseNode param) (parseNode paramType) (parseNode returnType) (parseNode body)
    | CST.identifier _ "lambda" :: param :: paramType :: returnType :: body :: [] =>
      AST.lambda loc (parseNode param) (parseNode paramType) (parseNode returnType) (parseNode body)
    | callee :: params => AST.apply loc (parseNode callee) (parseElements params)

  def parseMap (loc : Location) (elements : List CST) : AST :=
    AST.map loc (parseElements elements)

  def parseNode : CST → AST
    | CST.string loc s => AST.string loc s
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
  | string     : Location → String → SyntaxTree
  | list       : Location → List SyntaxTree → SyntaxTree
  | map        : Location → List SyntaxTree → SyntaxTree
  | lambda     : Location → (paramT : SyntaxTree) → (returnT : SyntaxTree) → (body : SyntaxTree) → SyntaxTree
  | macro      : Location → (paramT : SyntaxTree) → (returnT : SyntaxTree) → (body : SyntaxTree) → SyntaxTree
  | data       : Location → (deps : SyntaxTree) → (constructors : List SyntaxTree) → SyntaxTree
  | apply      : Location → (callee : SyntaxTree) → (args : List SyntaxTree) → SyntaxTree
  | definition : Location → (label : String) → (value : SyntaxTree) → SyntaxTree
  | varBound   : Location → (index : Nat) → SyntaxTree
  | varFree    : Location → (label : String) → SyntaxTree
  | error      : Location → String → SyntaxTree
  deriving Repr, Nonempty

mutual
  def parseLists (ctx : List String) : List AST → List SyntaxTree
    | [] => []
    | x :: xs => parse ctx x :: parseLists ctx xs

  def parse (ctx : List String) : AST → SyntaxTree
    | AST.string loc s => SyntaxTree.string loc s
    | AST.definition loc label body =>
      let label := parse ctx label
      match label with
      | SyntaxTree.varFree loc label =>
        let body := parse ctx body
        SyntaxTree.definition loc label body
      | _ => SyntaxTree.error loc "Trying to create a definition with a invalid node as label"
    | AST.list loc elements => SyntaxTree.list loc (parseLists ctx elements)
    | AST.map loc elements => SyntaxTree.map loc (parseLists ctx elements)
    | AST.lambda loc param paramT returnT body =>
      match param with
      | AST.identifier loc param =>
        let paramT := parse ctx paramT
        let ctx := ctx.cons param
        let returnT := parse ctx returnT
        let body := parse ctx body
        SyntaxTree.lambda loc paramT returnT body
      | _ => SyntaxTree.error loc "Using anything other than an identifier as parameter label is not supported"
    | AST.macro loc param paramT returnT body =>
      match param with
      | AST.identifier loc param =>
        let paramT := parse ctx paramT
        let ctx := ctx.cons param
        let returnT := parse ctx returnT
        let body := parse ctx body
        SyntaxTree.macro loc paramT returnT body
      | _ => SyntaxTree.error loc "Using anything other than an identifier as parameter label is not supported"
    | AST.data loc deps constructors =>
      SyntaxTree.data loc (parse ctx deps) (parseLists ctx constructors)
    | AST.identifier loc name =>
      match ctx.idxOf? name with
      | some i => SyntaxTree.varBound loc i
      | none => SyntaxTree.varFree loc name
    | AST.apply loc callee params => SyntaxTree.apply loc (parse ctx callee) (parseLists ctx params)
    | AST.error loc err => SyntaxTree.error loc err
end

end Lispoggers.Parser

namespace Lispoggers.Parser.Test

open Lispoggers.Lexer

def defaultContext : List String := ["a", "b"]

#eval "{a b a a}" |>
      lexer |>
      concreteParse |>
      abstractParse |>
      parse defaultContext
#eval "{a b b}" |> lexer
      |> concreteParse
      |> abstractParse
      |> parse defaultContext
#eval "(a b b a)" |>
      lexer |>
      concreteParse |>
      abstractParse |>
      parse defaultContext
#eval "(list a a b b)" |>
      lexer |>
      concreteParse |>
      abstractParse |>
      parse defaultContext
#eval "(data {a Type} (list true) (list false))" |>
      lexer |>
      concreteParse |>
      abstractParse |>
      parse defaultContext
#eval "((lambda x Type Type (x x)) (lambda x Type Type (x x)))" |>
      lexer |>
      concreteParse |>
      abstractParse |>
      parse defaultContext
#eval "(lambda a Type Type (lambda b Type Type (lambda c Type Type (a b c))))" |>
      lexer |>
      concreteParse |>
      abstractParse |>
      parse defaultContext
#eval "(lambda Bool Type Type (lambda x Bool Bool x))" |>
      lexer |>
      concreteParse |>
      abstractParse |>
      parse defaultContext
#eval "(lambda x Type Type x)" |>
      lexer |>
      concreteParse |>
      abstractParse |>
      parse defaultContext
#eval "(macro y Type Type (macro x y y x))" |>
      lexer |>
      concreteParse |>
      abstractParse |>
      parse defaultContext

end Lispoggers.Parser.Test
