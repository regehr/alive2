	.text
	.file	1 "foo.ll"
	.globl	test
	.type	test,@function
test:
	sub sp, sp, #16
	.loc 1 1 0
	b callee
	.size	test, .-test
