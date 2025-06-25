use regex::Regex;
use std::collections::{VecDeque};
use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;

mod lib;
use lib::{Instruction, VM};

fn read_lines<P>(filename: P) -> io::Result<io::BufReader<File>>
where P: AsRef<Path>, {
    let file = File::open(filename)?;
    Ok(io::BufReader::new(file))
}


fn parse_instructions(lines_iter: &mut dyn BufRead) -> VecDeque<Instruction> {
  let kekw_token_break_re = Regex::new(r"\s+").unwrap();
  let mut instructions = VecDeque::new();

  for line in lines_iter.lines().map_while(Result::ok) {
    let trimmed = line.trim();

    if trimmed.is_empty() { continue; }

    let mut tokens_iter =
      kekw_token_break_re
      .split(trimmed)
      .filter(|t| !t.is_empty())
      .peekable();

    match tokens_iter.next() {
      Some("pushInt") => {
        match tokens_iter.next() {
          Some(num) => match num.parse::<i32>() {
            Ok(value) => instructions.push_back(Instruction::PushInt(value)),
            Err(_) => println!("Line {}: Error: Expected integer after push", line),
          },
          _ => println!("Line {}: Error: Invalid push syntax", line),
        }
      },
      Some("label") => {
        match tokens_iter.next() {
          Some(label) => instructions.push_back(Instruction::Label(String::from(label))),
          _ => println!("Line {}: Error: Invalid label syntax", line),
        }
      },
      Some("jump") => {
        match tokens_iter.next() {
          Some(label) => instructions.push_back(Instruction::Jump(String::from(label))),
          _ => println!("Line {}: Error: Invalid label syntax", line),
        }
      },
      Some("add") => instructions.push_back(Instruction::Add),
      Some("ltF") => instructions.push_back(Instruction::LtF),
      Some("pop") => instructions.push_back(Instruction::Pop),
      Some("cmp") => instructions.push_back(Instruction::Cmp),
      Some("debug") => instructions.push_back(Instruction::Debug),
      Some(other) => println!("Line {}: \nError: Unknown instruction '{}'", line, other),
      _ => {}
    }
  }

  instructions

}

fn main() {
  let stdin = io::stdin();
  let mut reader = io::BufReader::new(stdin.lock());

  let mut _reader_fallback =
    read_lines("./bytecode.kekw")
    .unwrap_or_else(|e| {
      eprintln!("Error reading file: {}", e);
      panic!("Exiting due to error");
    });

  let program = parse_instructions(&mut reader);

  let mut vm = VM::new(program.clone());

  for _ in 0..150 {
    vm.step();
  }
}


#[cfg(test)]
mod tests {
  use super::*;
  use lib::Instruction;

  #[test]
  fn parse_instructions_test () {
    let mut lines = read_lines("./bytecode.kekw").unwrap();
    let instructions = parse_instructions(&mut lines);
    let expected = VecDeque::from([
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

    assert_eq!(instructions, expected);
  }

}
