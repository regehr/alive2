.text
.globl test
.type test,@function
test:
vsetvli a0, zero, e64, mf8, ta, ma
ret
.Lfunc_end0:
.size test,.-test
