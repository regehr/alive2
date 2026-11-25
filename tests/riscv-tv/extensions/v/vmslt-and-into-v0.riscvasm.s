.text
.globl test
.type test,@function
test:
vsetivli zero, 2, e8, m1, ta, mu
vle8.v v8, (a1)
vle8.v v9, (a2)
vle8.v v10, (a3)
vmslt.vv v0, v8, v9
vmslt.vv v0, v9, v10, v0.t
vmv.v.i v11, 0
vmerge.vim v11, v11, 1, v0
vse8.v v11, (a0)
ret
.Lfunc_end0:
.size test,.-test
