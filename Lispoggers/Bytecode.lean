import Lispoggers.Lexer
import Lispoggers.Parser

namespace Lispoggers.Bytecode

open Lispoggers.Lexer
open Lispoggers.Parser

inductive LiftedTree where
  | string     : Location → String → LiftedTree
  | list       : Location → List LiftedTree → LiftedTree
  | map        : Location → List LiftedTree → LiftedTree
  | apply      : Location → (callee : LiftedTree) → (args : List LiftedTree) → LiftedTree
  | varBound   : Location → (index : Nat) → LiftedTree
  | varFree    : Location → (label : String) → LiftedTree
  | error      : Location → String → LiftedTree
  deriving Repr, Nonempty

instance : ToString LiftedTree where
  toString :=
    let rec ts := λ
    | LiftedTree.string _ s => s
    | LiftedTree.map _ elements => s!"Map({elements.map ts})"
    | LiftedTree.list _ elements => s!"List({elements.map ts})"
    | LiftedTree.apply _ callee args => s!"Apply({ts callee}, {args.map ts})"
    | LiftedTree.varBound _ v => s!"Var({v})"
    | LiftedTree.varFree _ v => s!"Var({v})"
    | LiftedTree.error _ msg => s!"error: {msg}"
    ts

inductive TopLevelDefs where
  | lambda : (label : String) → (paramT : LiftedTree) → (returnT : LiftedTree) → (body : LiftedTree) → TopLevelDefs
  | macro  : (label : String) → (paramT : LiftedTree) → (returnT : LiftedTree) → (body : LiftedTree) → TopLevelDefs
  | value  : (label : String) → (value : LiftedTree) → TopLevelDefs
  deriving Repr, Nonempty

instance : ToString TopLevelDefs where
  toString :=
    let rec ts := λ
    | TopLevelDefs.lambda label paramT returnT body => s!"Lambda<{label}>({toString body}) : {toString paramT} => {toString returnT}"
    | TopLevelDefs.macro label paramT returnT body => s!"macro<{label}>({toString body}) : {toString paramT} => {toString returnT}"
    | TopLevelDefs.value label value => s!"Value<{label}>({toString value})"
    ts

mutual
  def lambdasLifting (defs : List TopLevelDefs) : List SyntaxTree → List TopLevelDefs × List LiftedTree
    | [] => (defs, [])
    | node :: nodes =>
      let (defs, node) := lambdaLifting defs node
      let (defs, nodes) := lambdasLifting defs nodes
      (defs, node :: nodes)

  def lambdaLifting (defs : List TopLevelDefs) : SyntaxTree → List TopLevelDefs × LiftedTree
    | SyntaxTree.string loc s =>
      let node := LiftedTree.string loc s
      (defs, node)
    | SyntaxTree.list loc elements =>
      let (defs, elements) := lambdasLifting defs elements
      let node := LiftedTree.list loc elements
      (defs, node)
    | SyntaxTree.map loc elements =>
      let (defs, elements) := lambdasLifting defs elements
      let node := LiftedTree.map loc elements
      (defs, node)
    | SyntaxTree.apply loc callee args =>
      let (defs, callee) := lambdaLifting defs callee
      let (defs, args) := lambdasLifting defs args
      let node := LiftedTree.apply loc callee args
      (defs, node)
    | SyntaxTree.varFree loc label =>
      let node := LiftedTree.varFree loc label
      (defs, node)
    | SyntaxTree.varBound loc index =>
      let node := LiftedTree.varBound loc index
      (defs, node)

    | SyntaxTree.definition _loc label value =>
      match value with
      | SyntaxTree.macro loc paramT returnT body =>
        let (defs, paramT) := lambdaLifting defs paramT
        let (defs, returnT) := lambdaLifting defs returnT
        let (defs, body) := lambdaLifting defs body
        let definition := TopLevelDefs.macro label paramT returnT body
        let node := LiftedTree.varFree loc label
        (definition :: defs, node)
      | SyntaxTree.lambda loc paramT returnT body =>
        let (defs, paramT) := lambdaLifting defs paramT
        let (defs, returnT) := lambdaLifting defs returnT
        let (defs, body) := lambdaLifting defs body
        let definition := TopLevelDefs.lambda label paramT returnT body
        let node := LiftedTree.varFree loc label
        (definition :: defs, node)
      | node =>
        let (defs, node) := lambdaLifting defs node
        let definition := TopLevelDefs.value label node
        (definition :: defs, node)

    | SyntaxTree.macro loc paramT returnT body =>
      let (defs, paramT) := lambdaLifting defs paramT
      let (defs, returnT) := lambdaLifting defs returnT
      let (defs, body) := lambdaLifting defs body

      let label := s!"__lifted_function_{defs.length}()"
      let definition := TopLevelDefs.lambda label paramT returnT body

      (definition :: defs, LiftedTree.varFree loc label)

    | SyntaxTree.lambda loc paramT returnT body =>
      let (defs, paramT) := lambdaLifting defs paramT
      let (defs, returnT) := lambdaLifting defs returnT
      let (defs, body) := lambdaLifting defs body

      let label := s!"__lifted_macro_{defs.length}()"
      let definition := TopLevelDefs.lambda label paramT returnT body

      (definition :: defs, LiftedTree.varFree loc label)

    | SyntaxTree.data loc _deps _constructors =>
      (defs, LiftedTree.error loc "Data node not implemented")
    | SyntaxTree.error loc err =>
      (defs, LiftedTree.error loc err)
end

#eval "(list a a b b)" |>
      lexer |>
      concreteParse |>
      abstractParse |>
      parse Lispoggers.Parser.Test.defaultContext |>
      lambdaLifting []
#eval "(def test (lambda x Type Type (x x)))" |>
      lexer |>
      concreteParse |>
      abstractParse |>
      parse Lispoggers.Parser.Test.defaultContext |>
      lambdaLifting []
#eval "((lambda x Type Type (x x)) (lambda x Type Type (x x)))" |>
      lexer |>
      concreteParse |>
      abstractParse |>
      parse Lispoggers.Parser.Test.defaultContext |>
      lambdaLifting []
#eval "(lambda Bool Type Type ((lambda x Bool Bool x) True))" |>
      lexer |>
      concreteParse |>
      abstractParse |>
      parse Lispoggers.Parser.Test.defaultContext |>
      lambdaLifting []
#eval "(lambda x Type Type x)" |>
      lexer |>
      concreteParse |>
      abstractParse |>
      parse Lispoggers.Parser.Test.defaultContext |>
      lambdaLifting []
#eval "(macro y Type Type (macro x y y x))" |>
      lexer |>
      concreteParse |>
      abstractParse |>
      parse Lispoggers.Parser.Test.defaultContext |>
      lambdaLifting []

inductive Instruction where
  | push  : Location → String → Instruction
  | pop   : Location → Instruction
  | store : Location → (register : Nat) → Instruction
  | load  : Location → (register : Nat) → Instruction
  | label : Location → (label : String) → Instruction
  | jump  : Location → (label : String) → Instruction
  | ret   : Location → Instruction
  | dump  : Location → Instruction
  deriving BEq, Repr, Nonempty

end Lispoggers.Bytecode
