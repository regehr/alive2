.text
.globl test
.type test,@function
test:
vsetivli zero, 2, e8, m1, ta, ma
vle8.v v8, (a0)
vmv.v.x v0, a1
vmv.s.x v9, zero
vredmaxu.vs v9, v8, v9, v0.t
vmv.x.s a0, v9
ret
.Lfunc_end0:
.size test,.-test
