.text
.globl test
.type test,@function
test:
  li a0, 0
  beqz a0, .Ldone
  addi sp, sp, -8
  addi sp, sp, 8
  .Ldone:
  li a0, 42
  ret
.Lfunc_end0:
.size test,.-test
