	.text
	.file	1 "foo.ll"
	.globl	test
	.type	test,@function
test:
	str d15, [sp, #-16]!
	str x30, [sp, #8]
	fmov d15, x0
	.loc 1 1 0
	bl clobber
	fmov x0, d15
	ldr x30, [sp, #8]
	ldr d15, [sp], #16
	ret
	.size	test, .-test
