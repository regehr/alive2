.text
.globl test
.type test,@function
test:
lui a0, %hi(p)
addi a0, a0, %lo(p)
ld a0, 8(a0)
ret
.Lfunc_end0:
.size test,.-test

.section .rodata
.p2align 3
p:
.byte 0
.quad g
.zero 7
.size p,.-p
