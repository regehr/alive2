.text
.globl test
.type test,@function
test:
vsetivli zero, 2, e32, mf2, ta, ma
vfmadd.vv v8, v9, v10
ret
.Lfunc_end0:
.size test,.-test
