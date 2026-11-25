.text
.globl test
.type test,@function
test:
vsetivli zero, 2, e16, m1, ta, ma
vle16.v v8, (a1)
vle16.v v9, (a2)
vmacc.vx v8, a3, v9
vse16.v v8, (a0)
ret
.Lfunc_end0:
.size test,.-test
