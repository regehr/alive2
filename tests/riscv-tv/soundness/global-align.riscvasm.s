.text
.globl test
.type test,@function
test:
lui a0, %hi(p)
addi a0, a0, %lo(p)
lbu a0, 1(a0)
ret
.Lfunc_end0:
.size test,.-test

.section .rodata
.p2align 3
p:
.byte 1
.p2align 3
.byte 2
.size p,.-p
