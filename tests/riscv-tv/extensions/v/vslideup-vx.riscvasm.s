.text
.globl test
.type test,@function
test:
vsetivli zero, 4, e8, m1, ta, ma
vle8.v v8, (a0)
vle8.v v9, (a1)
vslideup.vx v8, v9, a2
vse8.v v8, (a0)
ret
.Lfunc_end0:
.size test,.-test
