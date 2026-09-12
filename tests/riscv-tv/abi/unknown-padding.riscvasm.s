	.text
	.file	1 "foo.ll"
	.globl	test
	.type	test,@function
test:
	srli a0, a0, 13
	ret
.Lfunc_end0:
	.size	test, .-test
