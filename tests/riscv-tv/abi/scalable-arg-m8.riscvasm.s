.text
.globl test
.type test,@function
test:
vsetvli a0, zero, e64, m8, ta, ma
vand.vv v8, v8, v16
ret
.Lfunc_end0:
.size test,.-test
