.text
.globl test
.type test,@function
test:
vsetivli zero, 2, e8, m1, ta, ma
vle8.v v8, (a1)
vle8.v v9, (a2)
vmseq.vi v10, v8, 0
vmseq.vi v11, v9, 0
vmandn.mm v0, v10, v11
vmv.v.i v12, 0
vmerge.vim v12, v12, 1, v0
vse8.v v12, (a0)
ret
.Lfunc_end0:
.size test,.-test
