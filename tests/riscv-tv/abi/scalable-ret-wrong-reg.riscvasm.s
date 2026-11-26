.text
.globl test
.type test,@function
test:
vsetvli a0, zero, e32, m1, ta, ma
vadd.vv v9, v8, v9
ret
.Lfunc_end0:
.size test,.-test
