.text
.globl test
.type test,@function
test:
  addi sp, sp, -8
  addi sp, sp, 8
  li a0, 42
  ret
.Lfunc_end0:
.size test,.-test
