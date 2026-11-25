.text
.globl test
.type test,@function
test:
vsetivli zero, 1, e32, m1, ta, ma
csrr a0, vtype
ret
.Lfunc_end0:
.size test,.-test
