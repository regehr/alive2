.text
.globl test
.type test,@function
test:
vsetivli zero, 8, e32, m2, ta, ma
vle32.v v9, (a1)
vse32.v v9, (a0)
ret
.Lfunc_end0:
.size test,.-test
