.text
.globl test
.type test,@function
test:
vsetivli zero, 1, e16, m1, ta, ma
vle16.v v8, (a1)
vle16.v v9, (a2)
vle16.v v10, (a3)
vmadd.vv v8, v9, v10
vse16.v v8, (a0)
ret
.Lfunc_end0:
.size test,.-test
