import Lispoggers

def hello := "world"

def main : IO Unit :=
  IO.println s!"Hello, {hello}!"

#check main

#eval main
