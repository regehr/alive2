.text
.globl test
.type test,@function
test:
vsetivli zero, 4, e32, m1, ta, ma
vle32.v v8, (a0)
vmv.s.x v9, a1
vredsum.vs v9, v8, v9
vmv.x.s a0, v9
ret
.Lfunc_end0:
.size test,.-test
