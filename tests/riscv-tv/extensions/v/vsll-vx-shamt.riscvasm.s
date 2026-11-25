.text
.globl test
.type test,@function
test:
vsetivli zero, 2, e8, m1, ta, ma
vmv.v.i v8, 1
vsll.vx v8, v8, a1
vse8.v v8, (a0)
ret
.Lfunc_end0:
.size test,.-test
