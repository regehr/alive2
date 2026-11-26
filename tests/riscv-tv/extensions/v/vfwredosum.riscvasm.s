.text
.globl test
.type test,@function
test:
vsetivli zero, 1, e64, m1, ta, ma
vfmv.s.f v9, fa0
vsetivli zero, 1, e32, mf2, ta, ma
vfwredosum.vs v9, v8, v9
vsetivli zero, 1, e64, m1, ta, ma
vfmv.f.s fa0, v9
ret
.Lfunc_end0:
.size test,.-test
