	.text
	.file	1 "foo.ll"
	.globl	test
	.type	test,@function
test:
	slli a1, a1, 48
	srli a1, a1, 48
	xor a0, a0, a1
	ret
.Lfunc_end0:
	.size	test, .-test
