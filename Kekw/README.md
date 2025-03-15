# Kekw Virtual Machine

Stack based virtual machine

## Instructions

a stack with arithmetics and registers with boolean logic, labels and jumps;

```asm
push %100;
push %1;
label <label>;
push %2;
add;
cmp;       -- compares the first with the second elements of stack and updates the eq and lt registers
lt;        -- skips the next operation if the lt register is true 
eq;        -- skips the next operation if the eq register is true
jmp <label>;
debug;        -- stack := [100, 101] | lt := false | eq := false
```

