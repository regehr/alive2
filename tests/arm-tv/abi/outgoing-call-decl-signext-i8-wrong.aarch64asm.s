	.text
	.file	1 "foo.ll"
	.globl	test
	.type	test,@function
test:
	str x30, [sp, #-16]!
	mov w0, #255
	.loc 1 1 0
	bl consume
	ldr x30, [sp], #16
	ret
	.size	test, .-test
