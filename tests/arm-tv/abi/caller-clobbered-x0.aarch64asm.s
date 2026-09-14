	.text
	.file	1 "foo.ll"
	.globl	test
	.type	test,@function
test:
	str x30, [sp, #-16]!
	mov x0, #42
	.loc 1 1 0
	bl clobber
	mov x0, x0
	ldr x30, [sp], #16
	ret
	.size	test, .-test
