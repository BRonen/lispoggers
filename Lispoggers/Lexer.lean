def isWhitespace (c : Char) : Bool :=
  c == ' ' || c == '\n' || c == '\t' || c == ','

def isDigit (c : Char) : Bool :=
  c >= '0' && c <= '9'

def isAlpha (c : Char) : Bool :=
  (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

structure Location where
  line : Nat
  column : Nat
  size : Nat
  deriving Repr

inductive Token
  | lparen     : Location → Token
  | rparen     : Location → Token
  | lcurly     : Location → Token
  | rcurly     : Location → Token
  | identifier : Location → String → Token
  | error      : Location → String → Token
  deriving Repr

instance : ToString Token where
  toString := fun
    | Token.lparen _ => "("
    | Token.rparen _ => ")"
    | Token.lcurly _ => "{"
    | Token.rcurly _ => "}"
    | Token.identifier _ str => str
    | Token.error _ msg => s!"error: {msg}"

structure LexerState where
  acc : List Char
  tokens : List Token
  line : Nat
  column : Nat
  deriving Repr

def makeLocation (state : LexerState) (size := 1) : Location :=
  { line := state.line, column := state.column - size, size := size }

def flushIdentifier (state : LexerState) : List Token :=
  if state.acc.isEmpty then
    state.tokens
  else
    let loc := makeLocation state state.acc.length
    state.tokens ++ [Token.identifier loc (String.mk state.acc)]

def updateLocation (state : LexerState) (c : Char) : LexerState :=
  if c == '\n' then
    { state with line := state.line + 1, column := 0 }
  else
    { state with column := state.column + 1 }

def tokenize (state : LexerState) (h : Char) : LexerState :=
  if isWhitespace h then
    let tokens := flushIdentifier state
    let state := updateLocation state h
    { state with acc := [], tokens := tokens }
  else if isAlpha h then
    let state := updateLocation state h
    { state with acc := state.acc ++ [h] }
  else
    let tokens := flushIdentifier state
    let loc := makeLocation state
    let token := match h with
      | '(' => Token.lparen loc
      | ')' => Token.rparen loc
      | '{' => Token.lcurly loc
      | '}' => Token.rcurly loc
      | _ => Token.error loc s!"invalid character: {h}"
    let state := updateLocation state h
    { state with acc := [], tokens := tokens ++ [token] }

def lexer (input : String) : List Token :=
  let initialState : LexerState := {
    acc := [], tokens := [], line := 1, column := 0
  }
  let finalState := input.toList.foldl tokenize initialState
  flushIdentifier finalState

#eval lexer "
(add
 one
  two
   three)"
