.text
.globl test
.type test,@function
test:
vsetivli zero, 1, e8, m1, ta, ma
vle8.v v8, (a1)
vmulhsu.vx v8, v8, a2
vse8.v v8, (a0)
ret
.Lfunc_end0:
.size test,.-test
