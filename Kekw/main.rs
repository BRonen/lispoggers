use std::collections::{VecDeque};

mod lib;
use lib::{Instruction, VM};

fn main() {
  let program = VecDeque::from([
      Instruction::PushInt(3),
      Instruction::Label("teste".to_string()),
      Instruction::PushInt(2),
      Instruction::Add,
      Instruction::Debug,
      Instruction::PushInt(10),
      Instruction::Cmp,
      Instruction::Pop,
      Instruction::LtF,
      Instruction::Jump("teste".to_string()),
      Instruction::Debug,
  ]);

  let mut vm = VM::new(program.clone());

  for _ in 0..150 {
    vm.step();
  }
}

