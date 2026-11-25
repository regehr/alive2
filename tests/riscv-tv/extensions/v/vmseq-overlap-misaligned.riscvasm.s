.text
.globl test
.type test,@function
test:
vsetivli zero, 4, e32, m2, ta, ma
vmseq.vv v9, v8, v10
ret
.Lfunc_end0:
.size test,.-test
