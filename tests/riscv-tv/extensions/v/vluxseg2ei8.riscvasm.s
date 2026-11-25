.text
.globl test
.type test,@function
test:
vsetivli zero, 2, e8, m1, ta, ma
vle8.v v12, (a2)
vluxseg2ei8.v v8, (a1), v12
vse8.v v8, (a0)
vse8.v v9, (a3)
ret
.Lfunc_end0:
.size test,.-test
