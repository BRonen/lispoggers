import Lispoggers.Lexer
import Lispoggers.Parser

namespace Lispoggers.Bytecode

-- open Lispoggers.Lexer
-- open Lispoggers.Parser

-- inductive LiftedTree where
--   | string     : Location → String → LiftedTree
--   | list       : Location → List LiftedTree → LiftedTree
--   | map        : Location → List LiftedTree → LiftedTree
--   | apply      : Location → (callee : LiftedTree) → (args : List LiftedTree) → LiftedTree
--   | varBound   : Location → (index : Nat) → LiftedTree
--   | varFree    : Location → (label : String) → LiftedTree
--   | error      : Location → String → LiftedTree
--   deriving Repr, Nonempty

-- instance : ToString LiftedTree where
--   toString :=
--     let rec ts := λ
--     | LiftedTree.string _ s => s
--     | LiftedTree.map _ elements => s!"Map({elements.map ts})"
--     | LiftedTree.list _ elements => s!"List({elements.map ts})"
--     | LiftedTree.apply _ callee args => s!"Apply({ts callee}, {args.map ts})"
--     | LiftedTree.varBound _ v => s!"Var({v})"
--     | LiftedTree.varFree _ v => s!"Var({v})"
--     | LiftedTree.error _ msg => s!"error: {msg}"
--     ts

-- inductive TopLevelDef where
--   | lambda : (label : String) → (paramT : LiftedTree) → (returnT : LiftedTree) → (body : LiftedTree) → TopLevelDef
--   | macro  : (label : String) → (paramT : LiftedTree) → (returnT : LiftedTree) → (body : LiftedTree) → TopLevelDef
--   | value  : (label : String) → (value : LiftedTree) → TopLevelDef
--   deriving Repr, Nonempty

-- instance : ToString TopLevelDef where
--   toString :=
--     let rec ts := λ
--     | TopLevelDef.lambda label paramT returnT body => s!"Lambda<{label}>({toString body}) : {toString paramT} => {toString returnT}"
--     | TopLevelDef.macro label paramT returnT body => s!"macro<{label}>({toString body}) : {toString paramT} => {toString returnT}"
--     | TopLevelDef.value label value => s!"Value<{label}>({toString value})"
--     ts

-- mutual
--   def lambdasLifting (defs : List TopLevelDef) : List SyntaxTree → List TopLevelDef × List LiftedTree
--     | [] => (defs, [])
--     | node :: nodes =>
--       let (defs, node) := lambdaLifting defs node
--       let (defs, nodes) := lambdasLifting defs nodes
--       (defs, node :: nodes)

--   def lambdaLifting (defs : List TopLevelDef) : SyntaxTree → List TopLevelDef × LiftedTree
--     | SyntaxTree.string loc s =>
--       let node := LiftedTree.string loc s
--       (defs, node)
--     | SyntaxTree.list loc elements =>
--       let (defs, elements) := lambdasLifting defs elements
--       let node := LiftedTree.list loc elements
--       (defs, node)
--     | SyntaxTree.map loc elements =>
--       let (defs, elements) := lambdasLifting defs elements
--       let node := LiftedTree.map loc elements
--       (defs, node)
--     | SyntaxTree.apply loc callee args =>
--       let (defs, callee) := lambdaLifting defs callee
--       let (defs, args) := lambdasLifting defs args
--       let node := LiftedTree.apply loc callee args
--       (defs, node)
--     | SyntaxTree.varFree loc label =>
--       let node := LiftedTree.varFree loc label
--       (defs, node)
--     | SyntaxTree.varBound loc index =>
--       let node := LiftedTree.varBound loc index
--       (defs, node)

--     | SyntaxTree.definition _loc label value =>
--       match value with
--       | SyntaxTree.macro loc paramT returnT body =>
--         let (defs, paramT) := lambdaLifting defs paramT
--         let (defs, returnT) := lambdaLifting defs returnT
--         let (defs, body) := lambdaLifting defs body
--         let definition := TopLevelDef.macro label paramT returnT body
--         let node := LiftedTree.varFree loc label
--         (definition :: defs, node)
--       | SyntaxTree.lambda loc paramT returnT body =>
--         let (defs, paramT) := lambdaLifting defs paramT
--         let (defs, returnT) := lambdaLifting defs returnT
--         let (defs, body) := lambdaLifting defs body
--         let definition := TopLevelDef.lambda label paramT returnT body
--         let node := LiftedTree.varFree loc label
--         (definition :: defs, node)
--       | node =>
--         let (defs, node) := lambdaLifting defs node
--         let definition := TopLevelDef.value label node
--         (definition :: defs, node)

--     | SyntaxTree.macro loc paramT returnT body =>
--       let (defs, paramT) := lambdaLifting defs paramT
--       let (defs, returnT) := lambdaLifting defs returnT
--       let (defs, body) := lambdaLifting defs body

--       let label := s!"__lifted_function_{defs.length}()"
--       let definition := TopLevelDef.lambda label paramT returnT body

--       (definition :: defs, LiftedTree.varFree loc label)

--     | SyntaxTree.lambda loc paramT returnT body =>
--       let (defs, paramT) := lambdaLifting defs paramT
--       let (defs, returnT) := lambdaLifting defs returnT
--       let (defs, body) := lambdaLifting defs body

--       let label := s!"__lifted_macro_{defs.length}()"
--       let definition := TopLevelDef.lambda label paramT returnT body

--       (definition :: defs, LiftedTree.varFree loc label)

--     | SyntaxTree.data loc _deps _constructors =>
--       (defs, LiftedTree.error loc "Data node not implemented")
--     | SyntaxTree.error loc err =>
--       (defs, LiftedTree.error loc err)
-- end

-- #eval "(list a a b b)" |>
--       lexer |>
--       concreteParse |>
--       abstractParse |>
--       parse Lispoggers.Parser.Test.defaultContext |>
--       lambdaLifting []
-- #eval "(def test (lambda x Type Type (x x)))" |>
--       lexer |>
--       concreteParse |>
--       abstractParse |>
--       parse Lispoggers.Parser.Test.defaultContext |>
--       lambdaLifting []
-- #eval "((lambda x Type Type (x x)) (lambda x Type Type (x x)))" |>
--       lexer |>
--       concreteParse |>
--       abstractParse |>
--       parse Lispoggers.Parser.Test.defaultContext |>
--       lambdaLifting []
-- #eval "(lambda Bool Type Type ((lambda x Bool Bool x) True))" |>
--       lexer |>
--       concreteParse |>
--       abstractParse |>
--       parse Lispoggers.Parser.Test.defaultContext |>
--       lambdaLifting []
-- #eval "(lambda x Type Type x)" |>
--       lexer |>
--       concreteParse |>
--       abstractParse |>
--       parse Lispoggers.Parser.Test.defaultContext |>
--       lambdaLifting []
-- #eval "(macro y Type Type (macro x y y x))" |>
--       lexer |>
--       concreteParse |>
--       abstractParse |>
--       parse Lispoggers.Parser.Test.defaultContext |>
--       lambdaLifting []

-- namespace Lispoggers.Bytecode.Codegen

-- inductive Instruction where
--   | label : (label : String) → Instruction
--   | jump  : (label : String) → Instruction
--   | ret   : Instruction

--   | pushInt  : Int32 → Instruction
--   | pushBool : Bool → Instruction
--   | pushStr  : String → Instruction
--   | pop      : Instruction
--   | dup      : Instruction

--   | store : (addr : String) → Instruction
--   | load  : (addr : String) → Instruction

--   | add  : Instruction
--   | sub  : Instruction
--   | div  : Instruction
--   | mult : Instruction

--   | eq : Instruction
--   | lt : Instruction
--   | gt : Instruction
--   | or : Instruction
--   | not : Instruction
--   | and : Instruction
--   | sif : Instruction

--   | debug : Instruction
--   deriving BEq, Repr, Nonempty

-- mutual
--   def codegen_list : List LiftedTree → List (List Instruction)
--   | [] => []
--   | x :: xs => codegen x :: codegen_list xs

--   def codegen : LiftedTree → List Instruction
--   | LiftedTree.apply _ callee args =>
--       let args := codegen_list args
--       let args := List.flatten argInstrs

--       match callee with
--       | LiftedTree.varFree _ label =>
--           args ++ [Instruction.jump label]
--       | other =>
--           [Instruction.pushStr "Error while trying to apply a non-varFree", Instruction.debug]

--   | LiftedTree.varFree _ label =>
--       [Instruction.load label]

--   | LiftedTree.varBound _ idx =>
--       [Instruction.pushStr "Error while accessing a varBound", Instruction.debug]

--   | LiftedTree.string _ s =>
--       [Instruction.pushStr s]

--   | LiftedTree.error _ msg =>
--       [Instruction.debug]

--   | _ => [Instruction.pushStr "Error trying to codegen an invalid node", Instruction.debug]
-- end

-- def codegenDef : TopLevelDef → List Instruction
--   | TopLevelDef.lambda label _ _ body => [Instruction.label label] ++ codegen body ++ [Instruction.ret]
--   | TopLevelDef.macro label _ _ body => [Instruction.label label] ++ codegen body ++ [Instruction.ret]
--   | TopLevelDef.value label body => codegen body ++ [Instruction.store label]

end Lispoggers.Bytecode.Codegen
end Lispoggers.Bytecode
