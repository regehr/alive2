.text
.globl test
.type test,@function
test:
vsetivli zero, 2, e32, mf2, ta, ma
vfmin.vf v8, v8, fa0
ret
.Lfunc_end0:
.size test,.-test
