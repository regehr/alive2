.text
.globl test
.type test,@function
test:
vsetivli zero, 1, e8, m1, ta, ma
vle8.v v9, (a1)
vle8.v v10, (a2)
vrem.vv v8, v9, v10
vse8.v v8, (a0)
ret
.Lfunc_end0:
.size test,.-test
