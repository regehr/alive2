.text
.globl test
.type test,@function
test:
vsetivli zero, 2, e32, mf2, ta, ma
vle32.v v8, (a1)
vse32.v v8, (a0)
ret
.Lfunc_end0:
.size test,.-test
