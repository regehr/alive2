.text
.globl test
.type test,@function
test:
vsetivli zero, 1, e64, m1, ta, ma
vle64.v v8, (a0)
vmv.x.s a0, v8
ret
.Lfunc_end0:
.size test,.-test
