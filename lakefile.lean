import Lake
open Lake DSL

package lispoggers

lean_lib Lispoggers

@[default_target]
lean_exe lispoggers where
  root := `Main

require "leanprover-community" / "batteries" @ git "01006c9e86bf9e397c026fef4190478dd1fd897e"
