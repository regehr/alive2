	.text
	.file	1 "foo.ll"
	.globl	test
	.type	test,@function
test:
	srli a0, a0, 32
	srli a1, a1, 32
	xor a0, a0, a1
	ret
.Lfunc_end0:
	.size	test, .-test
