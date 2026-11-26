.text
.globl test
.type test,@function
test:
vsetvli a0, zero, e32, m1, ta, ma
vsub.vv v8, v12, v8
ret
.Lfunc_end0:
.size test,.-test
