.text
.globl test
.type test,@function
test:
vsetvli a0, a0, e32, m1, ta, ma
addi a0, a0, -3
sltiu a0, a0, 2
ret
.Lfunc_end0:
.size test,.-test
