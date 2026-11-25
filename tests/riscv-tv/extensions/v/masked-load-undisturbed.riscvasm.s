.text
.globl test
.type test,@function
test:
vsetivli zero, 16, e8, m1, ta, ma
vmv.v.x v0, a2
vsetivli zero, 2, e32, m1, ta, mu
vmv.v.i v8, 5
vle32.v v8, (a1), v0.t
vse32.v v8, (a0)
ret
.Lfunc_end0:
.size test,.-test
