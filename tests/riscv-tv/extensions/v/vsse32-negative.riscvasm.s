.text
.globl test
.type test,@function
test:
vsetivli zero, 2, e32, m1, ta, ma
vle32.v v8, (a1)
addi a0, a0, 4
li t0, -4
vsse32.v v8, (a0), t0
ret
.Lfunc_end0:
.size test,.-test
