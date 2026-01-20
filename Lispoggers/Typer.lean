import Lispoggers.Lexer
import Lispoggers.Parser

namespace Lispoggers.Typer

-- kinds

inductive K where
| star : K
| kind : K → K
deriving DecidableEq

open K

def sizeK
| star   => 1
| kind k => 1 + sizeK k

instance : ToString K where
  toString :=
    let rec f
    | star => "Star"
    | kind k => s!"Kind[{f k}]"
    f

-- types

inductive T where
| tbase   : String → T
| tvar    : Nat → T
| tapp    : T → T → T
| tarrow  : T → T → T
| tforall : K → T → T
deriving DecidableEq

open T

def sizeT
| tbase _     => 1
| tvar _      => 1
| tapp b p    => 1 + sizeK b + sizeT p
| tarrow a b  => 1 + sizeT a + sizeT b
| tforall k r => 1 + sizeK k + sizeT r

instance : ToString T where
  toString :=
    let rec f
    | tbase s     => s!"Base({s})"
    | tvar v      => s!"Var({v})"
    | tapp b p    => s!"({f b}. {f p})"
    | tarrow a b  => s!"(λ{f a}. {f b})"
    | tforall k r => s!"(∀{k}. {f r})"
    f

-- terms

inductive E where
| estr    : String → E
| eint    : Int → E
| evar    : Nat → E
| eapp    : E → E → E
| eapt    : E → T → E
| eabs    : T → E → E
| eforall : K → E → E

open E

def sizeE
| estr _      => 1
| eint _      => 1
| evar _      => 1
| eapp b a    => 1 + sizeE b + sizeE a
| eapt b t    => 1 + sizeE b + sizeT t
| eabs p b    => 1 + sizeT p + sizeE b
| eforall t b => 1 + sizeK t + sizeE b

instance : ToString E where
  toString :=
    let rec ts
    | estr    s   => s!"Str({s})"
    | eint    i   => s!"Int({i})"
    | evar    v   => s!"Var({v})"
    | eapp    f a => s!"({ts f} {ts a})"
    | eapt    t y => s!"[{ts t} {y}]"
    | eabs    t b => s!"(λ {t}. {ts b})"
    | eforall k b => s!"(∀ {k}. {ts b})"
    ts

-- checkers

def kindcheck (Δ : List K) (type : T) : Except String K :=
  match type with
  | tbase _ => pure star
  | tvar v => do
    match Δ[v]? with
    | some k => pure k
    | none => Except.error s!"Unbound type variable {v}"
  | tapp b p => do
    let b ← kindcheck Δ b
    let p ← kindcheck Δ p
    match b with
    | kind b =>
      if p = b
      then pure star
      else Except.error s!"Invalid application: {type}"
    | star => Except.error s!"Cannot apply callee: {type}"
  | tarrow t b => do
    let t ← kindcheck Δ t
    let b ← kindcheck Δ b
    match (t, b) with
    | (star, star) => pure star
    | (_, _) => Except.error s!"Wrong definition of arrow type: {t} → {b}"
  | tforall k b => do
    let b ← kindcheck (k :: Δ) b
    if b = star
    then pure star
    else Except.error s!"Wrong definition of forall type: {b} must be star"

def substT (term : T) (i : Nat) (v : T) : T :=
  match term with
  | tvar j =>
    if j = i
    then v
    else tvar j
  | tarrow a b =>
    let a := substT a i v
    let b := substT b i v
    tarrow a b
  | tapp b p =>
    let b := substT b i v
    let p := substT p i v
    tapp b p
  | tforall k b =>
    let b := substT b (i + 1) v
    tforall k b
  | t => t

def check (Γ : List T) (Δ : List K) (term : E) : Except String T :=
  match term with
  | eint _ => pure $ tbase "int"
  | estr _ => pure $ tbase "string"
  | evar v =>
    match Γ[v]? with
    | some v => pure v
    | none   => Except.error s!"Trying to access not defined variable: {v}"
  | eabs t b => do
    let b ← check (t :: Γ) Δ b
    pure $ tarrow t b
  | eforall k b => do
    let b ← check Γ (k :: Δ) b
    pure $ tforall k b
  | eapp f a => do
    let a ← check Γ Δ a
    let f ← check Γ Δ f
    match f with
    | tarrow t b =>
      if t = a
      then pure b
      else Except.error s!"Expected to apply {t} but received {a}"
    | _ => Except.error s!"Trying to apply value {a} on a term of type {f}"
  | eapt f a => do
    let a' ← kindcheck Δ a
    let f ← check Γ Δ f
    match f with
    | tforall k b =>
      if a' = k
      then pure (substT b 0 a)
      else Except.error s!"Trying to apply type {a} of {k} on a term of kind {f}"
    | _ => Except.error s!"Trying to apply value {a} on a term of type {f}"

-- type parser

open Lispoggers.Parser
open SyntaxTree

-- inductive SyntaxTree where
--   | string     : Location → String → SyntaxTree
--   | list       : Location → List SyntaxTree → SyntaxTree
--   | map        : Location → List SyntaxTree → SyntaxTree
--   | lambda     : Location → (paramT : SyntaxTree) → (returnT : SyntaxTree) → (body : SyntaxTree) → SyntaxTree
--   | macro      : Location → (paramT : SyntaxTree) → (returnT : SyntaxTree) → (body : SyntaxTree) → SyntaxTree
--   | data       : Location → (deps : SyntaxTree) → (constructors : List SyntaxTree) → SyntaxTree
--   | apply      : Location → (callee : SyntaxTree) → (args : List SyntaxTree) → SyntaxTree
--   | definition : Location → (label : String) → (value : SyntaxTree) → SyntaxTree
--   | varBound   : Location → (index : Nat) → SyntaxTree
--   | varFree    : Location → (label : String) → SyntaxTree
--   | error      : Location → String → SyntaxTree
--   deriving Repr, Nonempty

def typeParser : SyntaxTree → Except String E
| SyntaxTree.string _ s =>
    pure (estr s)
| SyntaxTree.varBound _ i =>
    pure (evar i)
| SyntaxTree.lambda _ paramT _ body => do
    -- For now, just assume every param is of type "string"
    let paramT := tbase "string"
    let body ← typeParser body
    pure (eabs paramT body)
| SyntaxTree.apply _ callee args => do
    let callee ← typeParser callee
    args.foldlM (fun f a => do
      let a ← typeParser a
      pure (eapp f a)) callee
| SyntaxTree.error _ msg =>
    Except.error msg
| st =>
    Except.error s!"Unsupported SyntaxTree in typeParser: {repr st}"

end Lispoggers.Typer
namespace Lispoggers.Typer.Test

open Lispoggers.Typer.K
open Lispoggers.Typer.T
open Lispoggers.Typer.E

#eval star
#eval kind (kind (kind star))

#eval (tbase "int")
#eval (tforall star (tarrow (tbase "int") (tbase "string")))
#eval (tforall (kind star) (tarrow (tbase "int") (tbase "string")))

#eval (eforall star (eabs (tbase "int") (estr "hello world")))
#eval (eforall (kind star) (eabs (tbase "int") (estr "hello world")))

#eval check [] [] (eapp (eabs (tbase "string") (eint 2)) (estr "hello world"))
#eval check [] [] (eforall star (eabs (tvar 0) (evar 0)))
#eval check [] [] (eforall star (eabs (tvar 1) (evar 1)))
#eval check [] [] (eforall star (eabs (tvar 0) (evar 1)))
#eval check [] [] (eapt (eforall star (eabs (tvar 0) (evar 0))) (tbase "int"))
#eval check [] [] (eapp (eapt (eforall star (eabs (tvar 0) (evar 0))) (tbase "int")) (eint 3))
#eval check [] [] (eapp (eabs (tbase "int") (eint 0)) (eapp (eapt (eforall star (eabs (tvar 0) (evar 0))) (tbase "int")) (eint 0)))
#eval check [] [] (eapp (eabs (tbase "string") (eint 0)) (eapp (eapt (eforall star (eabs (tvar 0) (evar 0))) (tbase "int")) (eint 0)))

end Lispoggers.Typer.Test
