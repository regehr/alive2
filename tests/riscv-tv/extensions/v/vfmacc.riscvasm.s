.text
.globl test
.type test,@function
test:
vsetivli zero, 2, e32, mf2, ta, ma
vfmacc.vv v10, v8, v9
vmv1r.v v8, v10
ret
.Lfunc_end0:
.size test,.-test
