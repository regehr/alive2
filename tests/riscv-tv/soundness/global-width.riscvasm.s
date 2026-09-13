.text
.globl test
.type test,@function
test:
lui a0, %hi(p)
ld a0, %lo(p)(a0)
ret
.Lfunc_end0:
.size test,.-test
.section .rodata
.p2align 3
p:
.word g
.word 1
.size p,.-p
