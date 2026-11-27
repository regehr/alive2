.text
.globl test
.type test,@function
test:
vsetivli zero, 8, e16, m1, ta, ma
vle16.v v8, (a1)
vandn.vx v8, v8, a2
vse16.v v8, (a0)
ret
.Lfunc_end0:
.size test,.-test
