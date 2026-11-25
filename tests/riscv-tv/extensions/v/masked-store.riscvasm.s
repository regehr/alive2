.text
.globl test
.type test,@function
test:
vsetivli zero, 16, e8, m1, ta, ma
vmv.v.x v0, a1
vsetivli zero, 2, e8, m1, ta, ma
vmv.v.i v8, 7
vse8.v v8, (a0), v0.t
ret
.Lfunc_end0:
.size test,.-test
