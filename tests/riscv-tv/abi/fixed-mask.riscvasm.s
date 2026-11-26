.text
.globl test
.type test,@function
test:
vsetivli zero, 4, e8, mf4, ta, ma
vmand.mm v0, v0, v8
ret
.Lfunc_end0:
.size test,.-test
