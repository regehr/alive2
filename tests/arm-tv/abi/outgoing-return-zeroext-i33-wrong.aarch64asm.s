	.text
	.file	1 "foo.ll"
	.globl	test
	.type	test,@function
test:
	mov x0, #1
	movk x0, #2, lsl #32
	ret
	.size	test, .-test
