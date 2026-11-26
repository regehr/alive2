.text
.globl test
.type test,@function
test:
addi sp, sp, -16
sd ra, 8(sp)
call produce
ld ra, 8(sp)
addi sp, sp, 16
ret
.Lfunc_end0:
.size test,.-test
