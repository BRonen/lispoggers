use regex::Regex;
use std::collections::{VecDeque};
use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;

mod kekw;
use kekw::{Instruction, VM};

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
      // Jumps
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
      Some("ret") => instructions.push_back(Instruction::Ret),

      // Pushes and pop
      Some("pushInt") => {
        match tokens_iter.next() {
          Some(num) => match num.parse::<i32>() {
            Ok(value) => instructions.push_back(Instruction::PushInt(value)),
            Err(_) => println!("Line {}: Error: Expected integer after push", line),
          },
          _ => println!("Line {}: Error: Invalid push syntax", line),
        }
      },
      Some("pushStr") => {
        match tokens_iter.next() {
          Some(s) => instructions.push_back(Instruction::PushStr(s.to_string())),
          _ => println!("Line {}: Error: Invalid push syntax", line),
        }
      },
      Some("pushBool") => {
        match tokens_iter.next() {
          Some(v) => match v.parse::<bool>() {
            Ok(value) => instructions.push_back(Instruction::PushBool(value)),
            Err(_) => println!("Line {}: Error: Expected integer after push", line),
          },
          _ => println!("Line {}: Error: Invalid push syntax", line),
        }
      },
      Some("pop") => instructions.push_back(Instruction::Pop),
      Some("dup") => instructions.push_back(Instruction::Dup),

      // Top Level definitions
      Some("store") => {
        match tokens_iter.next() {
          Some(s) => instructions.push_back(Instruction::Store(s.to_string())),
          _ => println!("Line {}: Error: Invalid push syntax", line),
        }
      },
      Some("load") => {
        match tokens_iter.next() {
          Some(s) => instructions.push_back(Instruction::Load(s.to_string())),
          _ => println!("Line {}: Error: Invalid push syntax", line),
        }
      },

      // Arithmetic
      Some("add") => instructions.push_back(Instruction::Add),
      Some("sub") => instructions.push_back(Instruction::Sub),
      Some("mult") => instructions.push_back(Instruction::Mult),
      Some("div") => instructions.push_back(Instruction::Div),

      // Boolean logic
      Some("eq") => instructions.push_back(Instruction::Eq),
      Some("lt") => instructions.push_back(Instruction::Lt),
      Some("gt") => instructions.push_back(Instruction::Gt),
      Some("or") => instructions.push_back(Instruction::Or),
      Some("not") => instructions.push_back(Instruction::Not),
      Some("and") => instructions.push_back(Instruction::And),
      Some("sif") => instructions.push_back(Instruction::Sif),

      // Debugging
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
  use kekw::Instruction;

  // #[test]
  // fn parse_instructions_test () {
  //   let mut lines = read_lines("./bytecode.kekw").unwrap();
  //   let instructions = parse_instructions(&mut lines);
  //   let expected = VecDeque::from([
  //     Instruction::PushInt(3),
  //     Instruction::Label("teste".to_string()),
  //     Instruction::PushInt(2),
  //     Instruction::Add,
  //     Instruction::Debug,
  //     Instruction::PushInt(10),
  //     Instruction::Cmp,
  //     Instruction::Pop,
  //     Instruction::LtF,
  //     Instruction::Jump("teste".to_string()),
  //     Instruction::Debug,
  //   ]);

  //   assert_eq!(instructions, expected);
  // }

}
