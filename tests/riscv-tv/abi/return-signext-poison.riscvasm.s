	.text
	.file	1 "foo.ll"
	.globl	test
	.type	test,@function
test:
	li a0, 255
	ret
.Lfunc_end0:
	.size	test, .-test
