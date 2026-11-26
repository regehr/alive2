.text
.globl test
.type test,@function
test:
vsetvli a0, zero, e32, m1, ta, ma
vmv1r.v v12, v24
vadd.vv v24, v8, v8
vmv1r.v v8, v24
vmv1r.v v24, v12
ret
.Lfunc_end0:
.size test,.-test
