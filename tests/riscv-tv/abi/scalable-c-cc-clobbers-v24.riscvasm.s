.text
.globl test
.type test,@function
test:
vsetvli a0, zero, e32, m1, ta, ma
vadd.vv v24, v8, v8
vmv1r.v v8, v24
ret
.Lfunc_end0:
.size test,.-test
