.text
.globl test
.type test,@function
test:
vsetivli zero, 1, e64, m1, ta, ma
vmv.x.s a1, v8
add a0, a0, a1
ret
.Lfunc_end0:
.size test,.-test
