.text
.globl test
.type test,@function
test:
vsetivli zero, 2, e32, mf2, ta, ma
vfmacc.vf v9, fa0, v8
vmv1r.v v8, v9
ret
.Lfunc_end0:
.size test,.-test
