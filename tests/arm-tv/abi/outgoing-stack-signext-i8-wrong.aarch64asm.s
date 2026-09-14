	.text
	.file	1 "foo.ll"
	.globl	test
	.type	test,@function
test:
	str x30, [sp, #-16]!
	sub sp, sp, #16
	mov w9, #255
	movk x9, #123, lsl #32
	str x9, [sp]
	.loc 1 1 0
	bl consume
	add sp, sp, #16
	ldr x30, [sp], #16
	ret
	.size	test, .-test
