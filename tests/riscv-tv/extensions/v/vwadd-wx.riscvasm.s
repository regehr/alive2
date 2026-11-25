.text
.globl test
.type test,@function
test:
vsetivli zero, 2, e16, m1, ta, ma
vle16.v v8, (a1)
vsetivli zero, 2, e8, mf2, ta, ma
vwadd.wx v8, v8, a2
vsetivli zero, 2, e16, m1, ta, ma
vse16.v v8, (a0)
ret
.Lfunc_end0:
.size test,.-test
