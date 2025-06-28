use std::fmt::{Display, Formatter, Result};
use std::collections::{HashMap, VecDeque};

#[derive(Clone, Debug, PartialEq)]
pub enum Instruction {
  // Jumps
  Label(String),
  Jump(String),
  Ret,

  // Pushes, pop
  PushInt(i32),
  PushBool(bool),
  PushStr(String),
  Pop,

  // Top Level definitions
  Store(String),
  Load(String),

  // Arithmetic
  Add,
  Sub,
  Mult,
  Div,

  // Boolean logic
  Eq,
  Lt,
  Gt,
  Or,
  Not,
  And,
  Sif,

  // Debugging
  Debug,
}

impl Display for Instruction {
  fn fmt(&self, f: &mut Formatter) -> Result {
    use Instruction::*;

    let instruction = match self {
      Label(label) => format!("<Jump {}>", label),
      Jump(label) => format!("<Jump {}>", label),
      Ret => "<Ret>".to_string(),

      PushInt(v) => format!("<PushInt {}>", v),
      PushBool(v) => format!("<PushBool {}>", v),
      PushStr(v) => format!("<PushStr {}>", v),
      Pop => "<Pop>".to_string(),

      Store(var) => format!("<Store {}>", var),
      Load(var) => format!("<Load {}>", var),

      Add  => "<Add>".to_string(),
      Sub  => "<Sub>".to_string(),
      Mult => "<Mult>".to_string(),
      Div  => "<Div>".to_string(),

      Eq  => "<Eq>".to_string(),
      Lt  => "<Lt>".to_string(),
      Gt  => "<Gt>".to_string(),
      Or  => "<Or>".to_string(),
      Not => "<Not>".to_string(),
      And => "<And>".to_string(),
      Sif => "<Sif>".to_string(),

      Debug => "<Debug>".to_string(),
    };

    f.write_str(&instruction)
  }
}

#[derive(Debug, Clone)]
pub enum Value {
  Int(i32),
  Str(String),
  Bool(bool)
}

impl Display for Value {
  fn fmt(&self, f: &mut Formatter) -> Result {
    let value = match self {
      Value::Int(v) => format!("Int({})", v),
      Value::Str(v) => format!("Str({})", v),
      Value::Bool(v) => format!("Bool({})", v),
    };

    f.write_str(&value)
  }
}

pub struct VM {
  instructions: VecDeque<Instruction>,
  labels: HashMap<String, usize>, // @TODO: replace every label by static addresses
  freemem: HashMap<String, Value>,
  stack: Vec<Value>,
  stacktrace: Vec<i32>,
  pc: usize,
}

impl VM {
  pub fn new(instructions: VecDeque<Instruction>) -> Self {
    Self {
      instructions,
      labels: HashMap::new(),
      freemem: HashMap::new(),
      stack: Vec::new(),
      stacktrace: Vec::new(),
      pc: 0,
    }
  }

  pub fn debug(&mut self) -> &mut Self {
    println!("=== Debug ===");
    println!("stack: {:?}", self.stack);
    println!("stacktrace: {:?}", self.stacktrace);
    println!("labels: {:?}", self.labels);
    println!("freemem: {:?}", self.freemem);
    println!("=== Debug ===");

    self
  }

  pub fn pop_two<F>(&mut self, func: F) -> &mut Self
  where
    F: FnOnce(&mut VM, Option<Value>, Option<Value>) -> ()
  {
    let f = self.stack.pop();
    let s = self.stack.pop();

    func(self, f, s);

    self
  }

  pub fn step(&mut self) -> &mut Self {
    self.pc = self.pc + 1;

    match self.instructions.get(self.pc - 1) {
      // Jumps,
      Some(Instruction::Label(label)) => {
        self.labels.insert(label.to_string(), self.pc.clone() - 1);
      },
      Some(Instruction::Jump(label)) => {
        let addr = self.labels.get(label).unwrap();
        self.pc = *addr;
      },

      // Pushes and pop
      Some(Instruction::Pop) => { self.stack.pop(); },
      Some(Instruction::PushInt(v)) => self.stack.push(Value::Int(*v)),
      Some(Instruction::PushStr(v)) => self.stack.push(Value::Str(v.to_string())),
      Some(Instruction::PushBool(v)) => self.stack.push(Value::Bool(*v)),

      // Top Level definitions
      Some(Instruction::Store(label)) => {
        match self.stack.pop() {
          Some(v) => {
            self.freemem.insert(label.to_string(), v);
          },
          _ => todo!(),
        }
      },
      Some(Instruction::Load(label)) => {
        let val = self.freemem.get(label).unwrap().clone();
        self.stack.push(val);
      },

      // Arithmetic
      Some(Instruction::Add) => {
        self.pop_two(|vm, f, s| {
          match (f, s) {
            (Some(Value::Int(f)), Some(Value::Int(s))) => vm.stack.push(Value::Int(f + s)),
            _ => todo!(),
          };
        });
      },
      Some(Instruction::Sub) => {
        self.pop_two(|vm, f, s| {
          match (f, s) {
            (Some(Value::Int(f)), Some(Value::Int(s))) => vm.stack.push(Value::Int(f - s)),
            _ => todo!(),
          };
        });
      },
      Some(Instruction::Mult) => {
        self.pop_two(|vm, f, s| {
          match (f, s) {
            (Some(Value::Int(f)), Some(Value::Int(s))) => vm.stack.push(Value::Int(f * s)),
            _ => todo!(),
          };
        });
      },
      Some(Instruction::Div) => {
        self.pop_two(|vm, f, s| {
          match (f, s) {
            (Some(Value::Int(f)), Some(Value::Int(s))) => vm.stack.push(Value::Int(f / s)),
            _ => todo!(),
          };
        });
      },

      // Boolean logic
      Some(Instruction::Eq) => {
        self.pop_two(|vm, f, s| {
          match (f, s) {
            (Some(Value::Bool(f)), Some(Value::Bool(s))) => vm.stack.push(Value::Bool(f == s)),
            (Some(Value::Str(f)), Some(Value::Str(s)))   => vm.stack.push(Value::Bool(f == s)),
            (Some(Value::Int(f)), Some(Value::Int(s)))   => vm.stack.push(Value::Bool(f == s)),
            _ => todo!(),
          }
        });
      },
      Some(Instruction::Lt) => {
        self.pop_two(|vm, f, s| {
          match (f, s) {
            (Some(Value::Bool(f)), Some(Value::Bool(s))) => vm.stack.push(Value::Bool(f < s)),
            (Some(Value::Str(f)), Some(Value::Str(s)))   => vm.stack.push(Value::Bool(f < s)),
            (Some(Value::Int(f)), Some(Value::Int(s)))   => vm.stack.push(Value::Bool(f < s)),
            _ => todo!(),
          }
        });
      },
      Some(Instruction::Gt) => {
        self.pop_two(|vm, f, s| {
          match (f, s) {
            (Some(Value::Bool(f)), Some(Value::Bool(s))) => vm.stack.push(Value::Bool(f > s)),
            (Some(Value::Str(f)), Some(Value::Str(s)))   => vm.stack.push(Value::Bool(f > s)),
            (Some(Value::Int(f)), Some(Value::Int(s)))   => vm.stack.push(Value::Bool(f > s)),
            _ => todo!(),
          }
        });
      },
      Some(Instruction::Or) => {
        self.pop_two(|vm, f, s| {
          match (f, s) {
            (Some(Value::Bool(f)), Some(Value::Bool(s))) => vm.stack.push(Value::Bool(f || s)),
            _ => todo!(),
          }
        });
      },
      Some(Instruction::And) => {
        self.pop_two(|vm, f, s| {
          match (f, s) {
            (Some(Value::Bool(f)), Some(Value::Bool(s))) => vm.stack.push(Value::Bool(f && s)),
            _ => todo!(),
          }
        });
      },
      Some(Instruction::Not) => {
        match self.stack.pop() {
          Some(Value::Bool(f)) => self.stack.push(Value::Bool(!f)),
          _ => todo!(),
        }
      },
      Some(Instruction::Sif) => {
        match self.stack.pop() {
          Some(Value::Bool(true)) => {
            self.pc += 1;
            //self.stack.push(Value::Bool(true));
          },
          Some(Value::Bool(false)) => {
            //self.stack.push(Value::Bool(false));
          },
          _ => todo!(),
        }
      },

      Some(Instruction::Debug) => {
        self.debug();
      },

      _ => {},
    }

    self
  }
}

#[no_mangle]
pub extern "C"
fn my_rust_function(arg: i32) -> i32 {
    arg * 3
}


#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn my_rust_function_test () {
    assert_eq!(my_rust_function(1), 3);
    assert_eq!(my_rust_function(2), 6);
    assert_eq!(my_rust_function(3), 9);
  }

}
