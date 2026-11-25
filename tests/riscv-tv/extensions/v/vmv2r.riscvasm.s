.text
.globl test
.type test,@function
test:
vl2re8.v v8, (a1)
vsetivli zero, 1, e8, m1, ta, ma
vmv2r.v v12, v8
vs2r.v v12, (a0)
ret
.Lfunc_end0:
.size test,.-test
