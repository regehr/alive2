.text
.globl test
.type test,@function
test:
  beqz a0, .Ldone
  addi sp, sp, -16
  addi sp, sp, 16
  .Ldone:
  li a0, 42
  ret
.Lfunc_end0:
.size test,.-test
