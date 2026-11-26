.text
.globl test
.type test,@function
test:
vsetvli a0, zero, e32, mf2, ta, ma
vxor.vv v8, v8, v9
ret
.Lfunc_end0:
.size test,.-test
