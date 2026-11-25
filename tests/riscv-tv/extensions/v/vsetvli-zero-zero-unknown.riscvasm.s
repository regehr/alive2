.text
.globl test
.type test,@function
test:
vsetvli zero, zero, e32, m1, ta, ma
csrr a0, vtype
ret
.Lfunc_end0:
.size test,.-test
