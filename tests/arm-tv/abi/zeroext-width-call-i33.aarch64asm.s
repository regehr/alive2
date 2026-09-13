	.text
	.file	1 "foo.ll"
	.globl	test
	.type	test,@function
test:
	str x30, [sp, #-16]!
	.loc 1 1 0
	bl produce
	ldr x30, [sp], #16
	ret
	.size	test, .-test
