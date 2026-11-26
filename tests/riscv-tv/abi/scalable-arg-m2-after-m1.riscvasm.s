.text
.globl test
.type test,@function
test:
vsetvli a0, zero, e32, m2, ta, ma
vadd.vv v8, v10, v10
ret
.Lfunc_end0:
.size test,.-test
