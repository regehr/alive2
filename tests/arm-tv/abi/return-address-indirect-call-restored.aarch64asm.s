	.text
	.file	1 "foo.ll"
	.globl	test
	.type	test,@function
test:
	stp x29, x30, [sp, #-16]!
	.loc 1 1 0
	blr x0
	ldp x29, x30, [sp], #16
	ret
	.size	test, .-test
