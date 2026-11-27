.text
.globl test
.type test,@function
test:
vsetivli zero, 4, e16, mf2, ta, ma
vle16.v v8, (a1)
vle16.v v9, (a2)
vwsll.vv v10, v8, v9
vsetivli zero, 4, e32, m1, ta, ma
vse32.v v10, (a0)
ret
.Lfunc_end0:
.size test,.-test
