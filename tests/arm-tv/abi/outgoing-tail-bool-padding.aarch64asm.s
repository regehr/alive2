	.text
	.file	1 "foo.ll"
	.globl	test
	.type	test,@function
test:
	mov w0, #257
	.loc 1 1 0
	b consume
	.size	test, .-test
