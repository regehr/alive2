.text
.globl test
.type test,@function
test:
vsetivli zero, 8, e8, mf2, ta, ma
vle8.v v8, (a1)
vwsll.vx v10, v8, a2
vsetivli zero, 8, e16, m1, ta, ma
vse16.v v10, (a0)
ret
.Lfunc_end0:
.size test,.-test
