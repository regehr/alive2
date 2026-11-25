.text
.globl test
.type test,@function
test:
vsetivli zero, 4, e64, m2, ta, ma
vle64.v v8, (a1)
vse64.v v8, (a0)
ret
.Lfunc_end0:
.size test,.-test
