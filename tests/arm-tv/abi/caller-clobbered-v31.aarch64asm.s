	.text
	.file	1 "foo.ll"
	.globl	test
	.type	test,@function
test:
	str x30, [sp, #-16]!
	movi v31.2d, #0
	.loc 1 1 0
	bl clobber
	umov x0, v31.d[1]
	ldr x30, [sp], #16
	ret
	.size	test, .-test
