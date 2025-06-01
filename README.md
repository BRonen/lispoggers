# LISPoggers

A statically typed poggers Lisp in Lean.
LISPoggers is an acronym for "Lisp Is Simply Poggers".

## Features

- [ ] Data types
  - [ ] Type constructors
  - [ ] Lists
  - [ ] Maps
- [ ] Pattern matching
- [ ] Functions
- [ ] Macros

# Algebraic Data types

Every structure can be represented by a data type, and every datatype is represented by a name, a map of dependencies identifiers its types.

```ts
(data <name> <dependencies>
  (<constructor> <type>*)*)
```

## Booleans

```ts
(data Bool {}
  (false)
  (true))
```

## Lists

```ts
(data List {a Type}
  (nil)
  (cons a (List a)))
```

## Maps

```ts
(data Map
  {k Type
   v Type}
  (empty (Map k v))
  (cons k v (Map k v)))
```

# Pattern matching

```ts
(match <expr>
  (<pattern> <expr>)
  (<pattern> <expr>))
```

## Matching over booleans

```ts
(match <expr>
  (false <expr>)
  (true <expr>))
```

# Functions

```ts
(lambda <name> <type>
  <expr>)
```

## Function application

```ts
(<name> <expr>)
```

# Macros

```ts
(macro <name> <type>
  <expr>)
```

## Macro application

```ts
(<name> <expr>)
```

# Complete BNF grammar

```ts
<expr> ::= <identifier> | <list> | <map>
<list> ::= "(" <divider> <expr> (<divider> <expr>)* <divider> ")"
<map> ::= "{" <divider> <expr> (<divider> <expr>)* <divider>"}"
<identifier> ::= ([a-z] | [A-Z])+
<divider> ::= (" " | "\n")*
```

# Compiler

```mermaid
flowchart TD
  A[Raw Source Code] -- > B[CST]
  B -- > C[AST]
  C -- > D[Macro Expansion]
  D -- > E[Source Compiled to KekwVM Bytecode]
  C -- > G[Type Checking Compiled to KekwVM Bytecode]
  C -.- J[Types Checking Interpreter]
  J -- > K[Type Checking Evaluation]
  K -- > I[Source Type Tree]
```

# Virtual Machine

```mermaid
flowchart TD
  L[Source as Bytecode] -- > M{KekwVM}
  M{KekwVM} -- > N[Code Execution]
  O[Type Checking as Bytecode] -- > M{KekwVM}
  M{KekwVM} -- > P[Type Tree]
```

