	.text
	.file	1 "foo.ll"
	.globl	test
	.type	test,@function
test:
	str x30, [sp, #-16]!
	mov x7, #42
	.loc 1 1 0
	bl clobber
	mov x0, x7
	ldr x30, [sp], #16
	ret
	.size	test, .-test
