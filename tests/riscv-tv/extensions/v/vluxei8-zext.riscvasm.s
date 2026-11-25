.text
.globl test
.type test,@function
test:
vsetivli zero, 2, e64, m1, ta, ma
vle8.v v9, (a2)
vluxei8.v v8, (a1), v9
vse64.v v8, (a0)
ret
.Lfunc_end0:
.size test,.-test
