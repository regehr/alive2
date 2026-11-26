.text
.globl test
.type test,@function
test:
vsetvli a0, zero, e64, m2, ta, ma
vmerge.vvm v8, v10, v8, v0
ret
.Lfunc_end0:
.size test,.-test
