.text
.globl test
.type test,@function
test:
  addi sp, sp, -16
  sd a0, 0(sp)
  ld a0, 0(sp)
  addi sp, sp, 16
  ret
.Lfunc_end0:
.size test,.-test
