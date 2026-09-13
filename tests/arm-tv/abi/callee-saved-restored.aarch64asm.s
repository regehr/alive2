	.text
	.globl	test
	.type	test,@function
test:
	stp x19, x28, [sp, #-32]!
	stp x29, x30, [sp, #16]
	mov x19, #0
	mov x28, #0
	mov x29, sp
	mov x30, #0
	ldp x29, x30, [sp, #16]
	ldp x19, x28, [sp], #32
	ret
	.size	test, .-test
