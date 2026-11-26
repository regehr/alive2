.text
.globl test
.type test,@function
test:
vsetvli a0, zero, e8, mf4, ta, ma
vmand.mm v0, v0, v8
ret
.Lfunc_end0:
.size test,.-test
