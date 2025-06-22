import Lispoggers

@[extern "my_rust_function"]
opaque myRustFunction : UInt32 → UInt32

def hello := "world"

def main : IO Unit := do
  let result := myRustFunction 2
  IO.println s!"Result from C: {result}"
  IO.println s!"Hello 1, {hello}!"

#check main
