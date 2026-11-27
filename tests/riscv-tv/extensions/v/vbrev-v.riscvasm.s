.text
.globl test
.type test,@function
test:
vsetivli zero, 2, e64, m1, ta, ma
vle64.v v8, (a1)
vbrev.v v8, v8
vse64.v v8, (a0)
ret
.Lfunc_end0:
.size test,.-test
