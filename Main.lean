import Lispoggers

open Lispoggers.Lexer
open Lispoggers.Parser
open Lispoggers.Bytecode

@[extern "my_rust_function"]
opaque myRustFunction : UInt32 → UInt32

def hello := "world"

partial def readAll (stdin : IO.FS.Stream) (bufSize : USize := 64 * 1024) : IO ByteArray := do
  let rec go (acc : ByteArray) := do
    let chunk ← stdin.read bufSize
    if chunk.isEmpty then
      pure acc
    else
      go (acc.append chunk)
  go ByteArray.empty

def main : IO Unit := do
  IO.println s!"Hello, {hello}!"

  let foreignResult := myRustFunction 2
  IO.println s!"Result from Rust: {foreignResult}"

  let stdin ← IO.getStdin
  let bytes ← readAll stdin
  let content := String.fromUTF8! bytes

  IO.println s!"Compiling result:"
  content |>
  lexer |>
  concreteParse |>
  abstractParse |>
  parse Lispoggers.Parser.Test.defaultContext |>
  lambdaLifting [] |>
  IO.println
