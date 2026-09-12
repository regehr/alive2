	.text
	.file	1 "foo.ll"
	.globl	test
	.type	test,@function
test:
	li a0, -1
	.loc 1 1 0
	tail consume
.Lfunc_end0:
	.size	test, .-test
