	.text
	.file	1 "foo.ll"
	.globl	test
	.type	test,@function
test:
	str x30, [sp, #-16]!
	mov x9, x0
	mov w0, #-1
	.loc 1 1 0
	blr x9
	ldr x30, [sp], #16
	ret
	.size	test, .-test
