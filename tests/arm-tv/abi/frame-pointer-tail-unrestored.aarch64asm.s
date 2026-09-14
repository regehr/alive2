	.text
	.file	1 "foo.ll"
	.globl	test
	.type	test,@function
test:
	mov x29, #0
	.loc 1 1 0
	b callee
	.size	test, .-test
