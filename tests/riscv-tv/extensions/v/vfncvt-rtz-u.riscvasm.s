.text
.globl test
.type test,@function
test:
vsetivli zero, 2, e32, mf2, ta, ma
vfncvt.rtz.xu.f.w v9, v8
vmv1r.v v8, v9
ret
.Lfunc_end0:
.size test,.-test
