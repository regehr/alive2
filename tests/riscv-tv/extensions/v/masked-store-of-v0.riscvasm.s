.text
.globl test
.type test,@function
test:
vsetivli zero, 4, e32, m1, ta, ma
vse32.v v0, (a0), v0.t
ret
.Lfunc_end0:
.size test,.-test
