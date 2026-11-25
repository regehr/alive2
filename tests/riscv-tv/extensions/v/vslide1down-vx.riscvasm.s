.text
.globl test
.type test,@function
test:
vsetivli zero, 3, e8, m1, ta, ma
vle8.v v8, (a0)
vslide1down.vx v9, v8, a1
vse8.v v9, (a0)
ret
.Lfunc_end0:
.size test,.-test
