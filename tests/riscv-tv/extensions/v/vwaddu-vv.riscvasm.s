.text
.globl test
.type test,@function
test:
vsetivli zero, 2, e8, mf2, ta, ma
vle8.v v8, (a1)
vle8.v v9, (a2)
vwaddu.vv v10, v8, v9
vsetivli zero, 2, e16, m1, ta, ma
vse16.v v10, (a0)
ret
.Lfunc_end0:
.size test,.-test
