import Lispoggers.Lexer
import Lispoggers.Parser

inductive SimpleTypedTree where
  | string : Location → String → SimpleTypedTree
  | type   : Location → String → SimpleTypedTree
  | list   : Location → List SimpleTypedTree → SimpleTypedTree
  | map    : Location → List SimpleTypedTree → SimpleTypedTree
  | lambda : Location → SimpleTypedTree → SimpleTypedTree → SimpleTypedTree
  | bind   : Location → Nat → SimpleTypedTree → SimpleTypedTree
  | error  : Location → String → SimpleTypedTree
  deriving BEq, Repr, Nonempty


mutual
  def simpleTypeParserList : List SimpleTypedTree → List SyntaxTree → List SimpleTypedTree
    | _, [] => []
    | ctx, x :: xs =>
      simpleTypeParse ctx x :: simpleTypeParserList ctx xs

  -- By now, the type only checks for wrong lambda definitions on the system.

  def simpleTypeParse (ctx : List SimpleTypedTree) (expr : SyntaxTree) : SimpleTypedTree :=
    match expr with
    | SyntaxTree.list loc elements =>
      SimpleTypedTree.list loc (simpleTypeParserList ctx elements)
    | SyntaxTree.map loc elements =>
      SimpleTypedTree.map loc (simpleTypeParserList ctx elements)
    | SyntaxTree.lambda loc t r b =>
      let t := simpleTypeParse ctx t
      let ctx' := ctx.cons t
      let r := simpleTypeParse ctx' r
      let b := simpleTypeParse ctx' b
      if r == b
      then SimpleTypedTree.lambda loc t b
      else SimpleTypedTree.error loc "Return Type Annotation Doesn't matches body's type"
    | SyntaxTree.string loc s =>
      SimpleTypedTree.string loc s
    | SyntaxTree.error loc e =>
      SimpleTypedTree.error loc e
    | SyntaxTree.bind loc i =>
      match ctx.get? i with
      | some t => t
      | none => SimpleTypedTree.error loc "Trying to access invalid index"
end

private def defaultContext : List SimpleTypedTree :=
  [(SimpleTypedTree.type ⟨0, 0, 0, ""⟩ "str"),
   (SimpleTypedTree.type ⟨0, 0, 0, ""⟩ "type")]

#eval simpleTypeParse defaultContext
      (SyntaxTree.lambda ⟨0, 0, 0, ""⟩
        (SyntaxTree.bind ⟨0, 0, 0, ""⟩ 0)
        (SyntaxTree.bind ⟨0, 0, 0, ""⟩ 1)
        (SyntaxTree.bind ⟨0, 0, 0, ""⟩ 1))

#eval simpleTypeParse defaultContext
      (SyntaxTree.list ⟨0, 0, 0, ""⟩
        [(SyntaxTree.string ⟨0, 0, 0, ""⟩ "first"),
         (SyntaxTree.bind ⟨0, 0, 0, ""⟩ 1),
         (SyntaxTree.lambda ⟨0, 0, 0, ""⟩
           (SyntaxTree.bind ⟨0, 0, 0, ""⟩ 0)
           (SyntaxTree.bind ⟨0, 0, 0, ""⟩ 1)
           (SyntaxTree.bind ⟨0, 0, 0, ""⟩ 1))])

#eval simpleTypeParse defaultContext
      (SyntaxTree.error ⟨0, 0, 0, ""⟩ "error test")

#eval simpleTypeParse defaultContext
      (SyntaxTree.list ⟨0, 0, 0, ""⟩ [(SyntaxTree.error ⟨0, 0, 0, ""⟩ "error test")])
