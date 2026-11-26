.text
.globl test
.type test,@function
test:
vsetivli zero, 2, e32, mf2, ta, ma
vfmv.s.f v9, fa0
vfredosum.vs v9, v8, v9
vfmv.f.s fa0, v9
ret
.Lfunc_end0:
.size test,.-test
