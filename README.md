# Lispoggers

A simple statically typed Lisp in Lean.

## Features

- [ ] Data types
  - [ ] Type constructors
  - [ ] Lists
  - [ ] Maps
- [ ] Pattern matching
- [ ] Functions
- [ ] Macros

# Data types

```ts
(data <name>
  (<constructor> <type>)
  (<constructor> <type>))
```

## Booleans

```ts
(data Bool ()
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
(data Map {k Type, v Type}
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
(def <name> <type>
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

# Complete grammar

```ts
<expr> ::= <identifier> | <list> | <map>
<list> ::= "(" <divider> <expr> (<divider> <expr>)* <divider> ")"
<map> ::= "{" <divider> <expr> (<divider> <expr>)* <divider>"}"
<identifier> ::= ([a-z] | [A-Z])+
<divider> ::= (" " | "\n")*
```

# Compiler

![Compiler Diagram](https://mermaid.ink/img/pako:eNqNkE1ugzAQha8y8jr0ACwqhR9VCcqmoC5ishiZCSDARsYORVHuXmOVtOqqG8tv5ps3T3NnQlXEQnbt1Swa1AaKpJQAe_6OM-TKakEQO-YCQfAKEY_z4rICkdcx33_r2OuEn1BoBenniHJqlfS9xPdS_rQbxranCoyCjLr54wTRYmhN8svqjRfL6OCGRNfK-j9TLwEc_dT0M3aQhvSoyb0eO3rz7I95esPeotkCZx46bIE9W2haF61ttmMD6QHbyl3uvlZKZhoaqGSh-1aou5KV8uE4tEblixQsNNrSjmll64aFV-wnp-xYoaGkxVrj8Ky6052V2vTjC31UiWA?type=png)

# Virtual Machine

![Virtual Machine Diagram](https://mermaid.ink/img/pako:eNqVjs0OgjAQhF9ls2d4AQ4eFE_-JhIPUg6bsvwEaUltg4Tw7rYavXvbmfl2MjNKXTImWN31KBsyFrJUKIB9ftHOSAZ6wHqyHLAC4ngFh3nH3Xg9LAH7iXd0zDceg-2TpbOtVkVATnk2DQybhmXXqvqvwvPnNzPMvivkGGHPpqe29KPn4Ai0DfcsMPFnSaYTKNTiOXJWXyYlMbHGcYRGu7rBpKL7wys3lGQ5bak21P_cgdRN669eXs3wYGs?type=png)

<!--
```
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
```
flowchart TD
  L[Source as Bytecode] -- > M{KekwVM}
  M{KekwVM} -- > N[Code Execution]
  O[Type Checking as Bytecode] -- > M{KekwVM}
  M{KekwVM} -- > P[Type Tree]
```

</p-->
