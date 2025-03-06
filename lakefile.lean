import Lake
open Lake DSL

package lispoggers

lean_lib Lispoggers

@[default_target]
lean_exe lispoggers where
  root := `Main

require kekwvm from "Kekwvm"
