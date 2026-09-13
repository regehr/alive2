	.text
	.file	1 "foo.ll"
	.globl	test
	.type	test,@function
test:
	str d8, [sp, #-16]!
	str x30, [sp, #8]
	movi v8.2d, #0
	.loc 1 1 0
	bl clobber
	umov x0, v8.d[1]
	ldr x30, [sp, #8]
	ldr d8, [sp], #16
	ret
	.size	test, .-test
