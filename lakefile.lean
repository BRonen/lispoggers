import Lake
open Lake DSL

package lispoggers

lean_lib Lispoggers


extern_lib "kekw" := do
  IO.println s!"Running Cargo build --release"
  let _ ← IO.Process.run {
    cmd := "cargo",
    args := #["build","--release"],
    cwd := __dir__
  }
  buildStaticLib ("target" / "release" / Lake.nameToStaticLib "kekw") #[]

@[default_target]
lean_exe lispoggers where
  root := `Main
  moreLinkArgs := #[
    "-L./target/release",
    "-lkekw"
  ]

require "leanprover-community" / "batteries" @ git "01006c9e86bf9e397c026fef4190478dd1fd897e"
