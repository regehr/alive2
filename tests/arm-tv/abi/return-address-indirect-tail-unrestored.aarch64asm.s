	.text
	.file	1 "foo.ll"
	.globl	test
	.type	test,@function
test:
	mov x30, #0
	.loc 1 1 0
	br x0
	.size	test, .-test
