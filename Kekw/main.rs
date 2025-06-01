use std::fmt::{Display, Formatter, Result};
use std::collections::{HashMap, VecDeque};

#[derive(Clone)]
enum Instruction {
  PushInt(i32),
  PushStr(String),
  Label(String),
  Jump(String),
  Debug,
  Pop,
  Add,
  Cmp,
  LtF,
  EqF,
}

impl Display for Instruction {
  fn fmt(&self, f: &mut Formatter) -> Result {
    use Instruction::*;

    let instruction = match self {
      PushInt(v) => format!("PushInt({})", v),
      PushStr(v) => format!("PushStr({})", v),
      Label(label) => format!("Label({})", label),
      Jump(label) => format!("Jump({})", label),
      Debug => String::from("Debug"),
      Pop => String::from("Pop"),
      Add => String::from("Add"),
      Cmp => String::from("Cmp"),
      LtF => String::from("Lt"),
      EqF => String::from("Eq"),
    };

    f.write_str(&instruction)
  }
}

#[derive(Debug)]
enum Value {
  Int(i32),
  Str(String),
}

impl Display for Value {
  fn fmt(&self, f: &mut Formatter) -> Result {
    use Value::*;

    let value = match self {
      Int(v) => format!("Int({})", v),
      Str(v) => format!("Str({})", v),
    };

    f.write_str(&value)
  }
}

struct VM {
  instructions: VecDeque<Instruction>,
  labels: HashMap<String, usize>,
  stack: Vec<Value>,
  lt: bool,
  eq: bool,
  pc: usize,
}

impl VM {
  pub fn new(instructions: VecDeque<Instruction>) -> Self {
    Self {
      instructions,
      labels: HashMap::new(),
      stack: Vec::new(),
      lt: false,
      eq: false,
      pc: 0,
    }
  }

  pub fn push_int(&mut self, v: i32) -> &mut Self {
    self.stack.push(Value::Int(v));

    self
  }

  pub fn push_str(&mut self, v: String) -> &mut Self {
    self.stack.push(Value::Str(v));

    self
  }

  pub fn label(&mut self, label: String) -> &mut Self {
    self.labels.insert(label, self.pc.clone() - 1);

    self
  }

  pub fn jump(&mut self, label: String) -> &mut Self {
    let addr = self.labels.get(&label).unwrap();
    self.pc = *addr;

    self
  }

  pub fn debug(&mut self) -> &mut Self {
    println!("=== Debug ===");
    println!("stack: {:?}", self.stack);
    println!("labels: {:?}", self.labels);
    println!("lt: {:?}", self.lt);
    println!("eq: {:?}", self.eq);
    println!("=== Debug ===");

    self
  }

  pub fn pop(&mut self) -> &mut Self {
    self.stack.pop();
    self
  }

  pub fn add(&mut self) -> &mut Self {
    let f = self.stack.pop();
    let s = self.stack.pop();

    match (f, s) {
      (Some(Value::Int(f)), Some(Value::Int(s))) => self.stack.push(Value::Int(f + s)),
      _ => todo!(),
    };

    self
  }

  pub fn cmp(&mut self) -> &mut Self {
    let b_value = self.stack.pop().unwrap();
    let a_value = self.stack.pop().unwrap();

    if let (Value::Int(a), Value::Int(b)) = (a_value, b_value) {
      let lt = a < b;
      let eq = a == b;

      self.lt = lt;
      self.eq = eq;

      self.stack.push(Value::Int(a));
      self.stack.push(Value::Int(b));
    }

    self
  }

  pub fn ltf(&mut self) -> &mut Self {
    if !self.lt {
      self.pc += 1;
    }

    self
  }

  pub fn eqf(&mut self) -> &mut Self {
    if !self.eq {
      self.pc += 1;
    }

    self
  }

  pub fn step(&mut self) -> &mut Self {
    self.pc = self.pc + 1;

    match self.instructions.get(self.pc - 1) {
      None => self,
      Some(Instruction::PushInt(v)) => self.push_int(*v),
      Some(Instruction::PushStr(v)) => self.push_str(v.to_string()),
      Some(Instruction::Label(label)) => self.label(label.to_string()),
      Some(Instruction::Jump(label)) => self.jump(label.to_string()),
      Some(Instruction::Debug) => self.debug(),
      Some(Instruction::Pop) => self.pop(),
      Some(Instruction::Add) => self.add(),
      Some(Instruction::Cmp) => self.cmp(),
      Some(Instruction::LtF) => self.ltf(),
      Some(Instruction::EqF) => self.eqf(),
    }
  }
}

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

