	.text
	.file	1 "foo.ll"
	.globl	test
	.type	test,@function
test:
	str d15, [sp, #-16]!
	str x30, [sp, #8]
	movi v15.2d, #0
	.loc 1 1 0
	bl clobber
	umov x0, v15.d[1]
	ldr x30, [sp, #8]
	ldr d15, [sp], #16
	ret
	.size	test, .-test
