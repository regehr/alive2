.text
.file 1 "foo.ll"
.globl test
.type test,@function
test:
  addi sp, sp, -8
  addi sp, sp, 8
  .loc 1 0 0
  tail callee
.Lfunc_end0:
.size test,.-test
