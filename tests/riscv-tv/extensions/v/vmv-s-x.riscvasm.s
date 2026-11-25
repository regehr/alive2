.text
.globl test
.type test,@function
test:
vsetivli zero, 1, e32, m1, ta, ma
vmv.s.x v8, a1
vse32.v v8, (a0)
ret
.Lfunc_end0:
.size test,.-test
