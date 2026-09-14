	.text
	.file	1 "foo.ll"
	.globl	test
	.type	test,@function
test:
	str x30, [sp, #-16]!
	movi v0.2d, #0
	.loc 1 1 0
	bl clobber
	fmov x0, d0
	ldr x30, [sp], #16
	ret
	.size	test, .-test
