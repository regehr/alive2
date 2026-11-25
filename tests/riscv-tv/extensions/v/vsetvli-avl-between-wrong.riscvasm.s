.text
.globl test
.type test,@function
test:
vsetvli a0, a0, e32, m1, ta, ma
ret
.Lfunc_end0:
.size test,.-test
