.text
.globl test
.type test,@function
test:
addi a0, zero, %lo(g)
ret
.Lfunc_end0:
.size test,.-test
