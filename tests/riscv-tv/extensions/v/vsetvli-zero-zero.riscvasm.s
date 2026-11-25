.text
.globl test
.type test,@function
test:
vsetivli zero, 3, e32, m1, ta, ma
vsetvli zero, zero, e64, m2, ta, ma
csrr a0, vl
ret
.Lfunc_end0:
.size test,.-test
