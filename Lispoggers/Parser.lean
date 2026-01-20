import Std.Data.HashMap
import Lispoggers.Lexer

namespace Lispoggers.Parser

open Std
open Lispoggers.Lexer

inductive CST where
| string     : Location → String   → CST
| integer    : Location → Int      → CST
| identifier : Location → String   → CST
| list       : Location → List CST → CST
| map        : Location → List CST → CST
| error      : Location → String   → CST
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
    | Token.integer loc int, frame :: rest =>
      { frame with
        elements := CST.integer loc int :: frame.elements } :: rest
    | Token.identifier loc str, frame :: rest =>
      { frame with
        elements := CST.identifier loc str :: frame.elements } :: rest
    | Token.lparen loc, stack =>
      ⟨[], loc, FrameKind.List⟩ :: stack
    | Token.lcurly loc, stack =>
      ⟨[], loc, FrameKind.Map⟩ :: stack
    | Token.rparen _, frame :: parent :: rest =>
      let cst := match frame.kind with
      | FrameKind.List => CST.list frame.loc frame.elements.reverse
      | FrameKind.Map => CST.error frame.loc "Mismatched brackets"
      { parent with
        elements := cst :: parent.elements } :: rest
    | Token.rcurly _, frame :: parent :: rest =>
      let cst := match frame.kind with
      | FrameKind.Map => CST.map frame.loc frame.elements.reverse
      | FrameKind.List => CST.error frame.loc "Mismatched brackets"
      { parent with
        elements := cst :: parent.elements } :: rest
    | Token.error loc msg, frame :: rest =>
      { frame with
        elements := CST.error loc msg :: frame.elements } :: rest
    | _, _ =>
      [⟨ [CST.error ⟨0, 0, 0, ""⟩ "Parse error"]
       , ⟨0, 0, 0, ""⟩
       , FrameKind.List ⟩]

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
| integer    : Location → Int → AST
| list       : Location → List AST → AST
| map        : Location → List AST → AST
| definition : Location → (label : AST) → (value : AST) → AST
| lambda     : Location → (param : AST) → (paramType : AST) → (returnType : AST) → (body : AST) → AST
| apply      : Location → (callee : AST) → (arg : List AST) → AST
| defmacro   : Location → (label : String) → (param : AST) → (paramType : AST) → (returnType : AST) → (body : AST) → AST
| applym     : Location → (callee : String) → (arg : List CST) → AST
| identifier : Location → String → AST
| error      : Location → String → AST
deriving Repr

def MacroEnv := HashMap String AST

mutual
   def parseElements (macroEnv : MacroEnv) : List CST → List AST × MacroEnv
    | [] => ([], macroEnv)
    | x :: xs =>
      let (x', macroEnv') := partialParse macroEnv x
      let (xs', macroEnv'') := parseElements macroEnv' xs
      (x' :: xs', macroEnv'')

  def parseList (loc : Location) (macroEnv : HashMap String AST) : List CST → AST × MacroEnv
    | [] => (AST.error loc "Empty form found", macroEnv)
    | CST.identifier _ "list" :: elements =>
      let (elements', macroEnv') := parseElements macroEnv elements
      (AST.list loc elements', macroEnv')
    | CST.identifier _ "def" :: label :: element :: [] =>
      let (label', macroEnv') := partialParse macroEnv label
      let (element', macroEnv'') := partialParse macroEnv' element
      (AST.definition loc label' element', macroEnv'')
    | CST.identifier _ "lambda" :: param :: paramType :: returnType :: body :: [] =>
      let (param', macroEnv') := partialParse macroEnv param
      let (paramType', macroEnv'') := partialParse macroEnv' paramType
      let (returnType', macroEnv''') := partialParse macroEnv'' returnType
      let (body', macroEnv'''') := partialParse macroEnv''' body
      (AST.lambda loc param' paramType' returnType' body', macroEnv'''')
    | CST.identifier loc "defmacro" :: CST.string _ label :: param :: paramType :: returnType :: body :: [] =>
      let (param', macroEnv') := partialParse macroEnv param
      let (paramType', macroEnv'') := partialParse macroEnv' paramType
      let (returnType', macroEnv''') := partialParse macroEnv'' returnType
      let (body', macroEnv'''') := partialParse macroEnv''' body
      let defmacro := AST.defmacro loc label param' paramType' returnType' body'
      let macroEnv''''' := macroEnv''''.insert label defmacro
      (defmacro, macroEnv''''')
    | callee :: params =>
      match callee with
      | CST.identifier loc iden =>
        if macroEnv.contains iden
        then (AST.applym loc iden params, macroEnv)
        else
          let (callee', macroEnv') := partialParse macroEnv callee
          let (params', macroEnv'') := parseElements macroEnv' params
          (AST.apply loc callee' params', macroEnv'')
      | _ =>
        let (callee', macroEnv') := partialParse macroEnv callee
        let (params', macroEnv'') := parseElements macroEnv' params
        (AST.apply loc callee' params', macroEnv'')

  def parseMap (loc : Location) (macroEnv : HashMap String AST) (elements : List CST) : (AST × MacroEnv) :=
    let (elements', macroEnv') := parseElements macroEnv elements
    (AST.map loc elements', macroEnv')

  def partialParse (macroEnv : HashMap String AST) : CST → AST × MacroEnv
  | CST.string loc s => (AST.string loc s, macroEnv)
  | CST.integer loc n => (AST.integer loc n, macroEnv)
  | CST.identifier loc name => (AST.identifier loc name, macroEnv)
  | CST.error loc msg => (AST.error loc msg, macroEnv)
  | CST.map loc elements => parseMap loc macroEnv elements
  | CST.list loc elements => parseList loc macroEnv elements
end

-- def expand (ast : AST) (env : List AST) : AST :=
--   match ast with
--   | AST.macro loc param paramType returnType body =>
--     let env := env.addMacro ast
--     AST.macro loc param paramType returnType body
--   | AST.applym loc callee cstArgs =>
--     let callee' := expand callee env
--     match env.getMacro callee' with
--     | none => AST.error loc "Not a macro"
--     | some m =>
--       let resultCST := evalMacro m cstArgs
--       let parsedAST := parseCSTtoAST resultCST
--       expand parsedAST env
--   | AST.apply loc callee args =>
--     let callee' := expand callee env
--     let args'   := args.map (fun a => expand a env)
--     AST.apply loc callee' args'
--   | AST.lambda loc param paramType returnType body =>
--     let param'      := expand param env
--     let paramType'  := expand paramType env
--     let returnType' := expand returnType env
--     let body'       := expand body env
--     AST.lambda loc param' paramType' returnType' body'
--   | AST.definition loc label value =>
--     let label' := expand label env
--     let value' := expand value env
--     AST.definition loc label' value'

inductive ExpandedAST where
  | string     : Location → String → ExpandedAST
  | integer    : Location → Int → ExpandedAST
  | list       : Location → List ExpandedAST → ExpandedAST
  | map        : Location → List ExpandedAST → ExpandedAST
  | definition : Location → (label : ExpandedAST) → (value : ExpandedAST) → ExpandedAST
  | apply      : Location → (callee : ExpandedAST) → (arg : List ExpandedAST) → ExpandedAST
  | lambda     : Location → (param : ExpandedAST) → (paramType : ExpandedAST) → (returnType : ExpandedAST) → (body : ExpandedAST) → ExpandedAST
  | identifier : Location → String → ExpandedAST
  | error      : Location → String → ExpandedAST
  deriving Repr

-- mutual
--   def parseElements : List CST → List ExpandedAST
--     | [] => []
--     | x :: xs => parseNode x :: parseElements xs

--   def parseList (loc : Location) : List CST → ExpandedAST
--     | [] => ExpandedAST.list loc []
--     | CST.identifier _ "list" :: elements =>
--       ExpandedAST.list loc (parseElements elements)
--     | CST.identifier _ "def" :: label :: element :: [] =>
--       ExpandedAST.definition loc (parseNode label) (parseNode element)
--     | CST.identifier _ "lambda" :: param :: paramType :: returnType :: body :: [] =>
--       ExpandedAST.lambda loc (parseNode param) (parseNode paramType) (parseNode returnType) (parseNode body)
--     | callee :: params => ExpandedAST.apply loc (parseNode callee) (parseElements params)

--   def parseMap (loc : Location) (elements : List CST) : ExpandedAST :=
--     ExpandedAST.map loc (parseElements elements)

--   def parseNode : CST → ExpandedAST
--     | CST.string loc s => ExpandedAST.string loc s
--     | CST.integer loc i => ExpandedAST.integer loc i
--     | CST.identifier loc str => ExpandedAST.identifier loc str
--     | CST.list loc elements => parseList loc elements
--     | CST.map loc elements => parseMap loc elements
--     | CST.error loc msg => ExpandedAST.error loc msg
-- end

-- def abstractParse : CST → ExpandedAST := parseNode

-- #eval "(lambda x Bool Bool x)" |>
--       lexer |>
--       concreteParse |>
--       abstractParse
-- #eval "{a b c d}" |>
--       lexer |>
--       concreteParse |>
--       abstractParse
-- #eval "{a b c}" |>
--       lexer |>
--       concreteParse |>
--       abstractParse
-- #eval "(f x y)" |>
--       lexer |>
--       concreteParse |>
--       abstractParse

-- inductive AlphaTree where
--   | string     : Location → String → AlphaTree
--   | integer    : Location → Int → AlphaTree
--   | list       : Location → List AlphaTree → AlphaTree
--   | map        : Location → List AlphaTree → AlphaTree
--   | lambda     : Location → (paramT : AlphaTree) → (returnT : AlphaTree) → (body : AlphaTree) → AlphaTree
--   | apply      : Location → (callee : AlphaTree) → (args : List AlphaTree) → AlphaTree
--   | definition : Location → (label : String) → (value : AlphaTree) → AlphaTree
--   | varBound   : Location → (index : Nat) → AlphaTree
--   | varFree    : Location → (label : String) → AlphaTree
--   | error      : Location → String → AlphaTree
--   deriving Repr, Nonempty

-- mutual
--   def alphaLists (ctx : List String) : List ExpandedAST → List AlphaTree
--     | [] => []
--     | x :: xs => alpha ctx x :: alphaLists ctx xs

--   def alpha (ctx : List String) : ExpandedAST → AlphaTree
--     | ExpandedAST.string loc s => AlphaTree.string loc s
--     | ExpandedAST.integer loc i => AlphaTree.integer loc i
--     | ExpandedAST.definition loc label body =>
--       let label := alpha ctx label
--       match label with
--       | AlphaTree.varFree loc label =>
--         let body := alpha ctx body
--         AlphaTree.definition loc label body
--       | _ => AlphaTree.error loc "Trying to create a definition with a invalid node as label"
--     | ExpandedAST.list loc elements => AlphaTree.list loc (alphaLists ctx elements)
--     | ExpandedAST.map loc elements => AlphaTree.map loc (alphaLists ctx elements)
--     | ExpandedAST.lambda loc param paramT returnT body =>
--       match param with
--       | ExpandedAST.identifier loc param =>
--         let paramT := alpha ctx paramT
--         let ctx := ctx.cons param
--         let returnT := alpha ctx returnT
--         let body := alpha ctx body
--         AlphaTree.lambda loc paramT returnT body
--       | _ => AlphaTree.error loc "Using anything other than an identifier as parameter label is not supported"
--     | ExpandedAST.identifier loc name =>
--       match ctx.idxOf? name with
--       | some i => AlphaTree.varBound loc i
--       | none => AlphaTree.varFree loc name
--     | ExpandedAST.apply loc callee params => AlphaTree.apply loc (alpha ctx callee) (alphaLists ctx params)
--     | ExpandedAST.error loc err => AlphaTree.error loc err
-- end

end Lispoggers.Parser
-- namespace Lispoggers.Parser.Test

-- open Lispoggers.Lexer

-- def defaultContext : List String := ["a", "b"]

-- #eval "{a b a a}" |>
--       lexer |>
--       concreteParse |>
--       abstractParse |>
--       alpha defaultContext
-- #eval "{a b b}" |>
--       lexer |>
--       concreteParse |>
--       abstractParse |>
--       alpha defaultContext
-- #eval "(a b b a)" |>
--       lexer |>
--       concreteParse |>
--       abstractParse |>
--       alpha defaultContext
-- #eval "(list a a b b)" |>
--       lexer |>
--       concreteParse |>
--       abstractParse |>
--       alpha defaultContext
-- #eval "((lambda x Type Type (x x)) (lambda x Type Type (x x)))" |>
--       lexer |>
--       concreteParse |>
--       abstractParse |>
--       alpha defaultContext
-- #eval "(lambda a Type Type (lambda b Type Type (lambda c Type Type (a b c))))" |>
--       lexer |>
--       concreteParse |>
--       abstractParse |>
--       alpha defaultContext
-- #eval "(lambda Bool Type Type (lambda x Bool Bool x))" |>
--       lexer |>
--       concreteParse |>
--       abstractParse |>
--       alpha defaultContext
-- #eval "(lambda x Type Type x)" |>
--       lexer |>
--       concreteParse |>
--       abstractParse |>
--       alpha defaultContext

-- end Lispoggers.Parser.Test
