.text
.globl test
.type test,@function
test:
vsetivli zero, 2, e16, mf4, ta, ma
vfwcvt.f.f.v v9, v8
vmv1r.v v8, v9
ret
.Lfunc_end0:
.size test,.-test
