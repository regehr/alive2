.text
.globl test
.type test,@function
test:
vsetivli zero, 2, e8, m1, ta, mu
vmv.v.x v0, a2
vle8.v v8, (a1)
vadd.vi v8, v8, 1, v0.t
vse8.v v8, (a0)
ret
.Lfunc_end0:
.size test,.-test
