.text
.globl test
.type test,@function
test:
vsetivli zero, 2, e8, m1, ta, ma
vle8.v v8, (a1)
vle8.v v9, (a2)
vle8.v v10, (a3)
vsseg3e8.v v8, (a0)
ret
.Lfunc_end0:
.size test,.-test
