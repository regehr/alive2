.text
.globl test
.type test,@function
test:
vsetivli zero, 1, e16, m1, ta, ma
vle16.v v8, (a1)
vsetivli zero, 1, e8, mf2, ta, ma
vle8.v v10, (a2)
vwmaccus.vx v8, a3, v10
vsetivli zero, 1, e16, m1, ta, ma
vse16.v v8, (a0)
ret
.Lfunc_end0:
.size test,.-test
