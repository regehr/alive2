.text
.globl test
.type test,@function
test:
vsetivli zero, 2, e32, m1, ta, ma
li t0, 8
vlse32.v v8, (a1), t0
vse32.v v8, (a0)
ret
.Lfunc_end0:
.size test,.-test
