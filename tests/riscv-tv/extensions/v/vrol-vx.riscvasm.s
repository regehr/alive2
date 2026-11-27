.text
.globl test
.type test,@function
test:
vsetivli zero, 4, e32, m1, ta, ma
vle32.v v8, (a1)
vrol.vx v8, v8, a2
vse32.v v8, (a0)
ret
.Lfunc_end0:
.size test,.-test
