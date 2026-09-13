	.text
	.file	1 "foo.ll"
	.globl	test
	.type	test,@function
test:
	str d8, [sp, #-16]!
	str x30, [sp, #8]
	fmov d8, x0
	.loc 1 1 0
	bl clobber
	fmov x0, d8
	ldr x30, [sp, #8]
	ldr d8, [sp], #16
	ret
	.size	test, .-test
