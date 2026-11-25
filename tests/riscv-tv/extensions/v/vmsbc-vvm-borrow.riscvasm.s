.text
.globl test
.type test,@function
test:
vsetivli zero, 1, e8, m1, ta, ma
vle8.v v8, (a1)
vle8.v v10, (a2)
vmv.v.x v0, a3
vmsbc.vvm v9, v8, v10, v0
vmv1r.v v0, v9
vmv.v.i v11, 0
vmerge.vim v11, v11, 1, v0
vse8.v v11, (a0)
ret
.Lfunc_end0:
.size test,.-test
