.text
.globl test
.type test,@function
test:
vsetivli zero, 4, e16, mf2, ta, ma
vle16.v v8, (a1)
vwsll.vi v9, v8, 20
vsetivli zero, 4, e32, m1, ta, ma
vse32.v v9, (a0)
ret
.Lfunc_end0:
.size test,.-test
