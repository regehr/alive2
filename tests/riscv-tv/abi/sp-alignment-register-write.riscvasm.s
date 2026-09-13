.text
.globl test
.type test,@function
test:
  mv t0, sp
  addi t1, sp, -8
  mv sp, t1
  mv sp, t0
  li a0, 42
  ret
.Lfunc_end0:
.size test,.-test
