	.text
	.file	1 "foo.ll"
	.globl	test
	.type	test,@function
test:
	mov w0, #255
	.loc 1 1 0
	b consume
	.size	test, .-test
