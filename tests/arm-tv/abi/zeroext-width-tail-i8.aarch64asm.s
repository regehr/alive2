	.text
	.file	1 "foo.ll"
	.globl	test
	.type	test,@function
test:
	.loc 1 1 0
	b produce
	.size	test, .-test
