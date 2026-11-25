.text
.globl test
.type test,@function
test:
vsetivli zero, 4, e8, m1, tu, ma
vle8.v v8, (a1)
vmsne.vi v9, v8, 0
vmv.v.i v10, 0
vcompress.vm v10, v8, v9
vse8.v v10, (a0)
ret
.Lfunc_end0:
.size test,.-test
