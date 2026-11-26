.text
.globl test
.type test,@function
test:
vsetivli zero, 2, e32, mf2, ta, ma
vfcvt.x.f.v v8, v8
ret
.Lfunc_end0:
.size test,.-test
