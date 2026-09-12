	.text
	.file	1 "foo.ll"
	.globl	test
	.type	test,@function
test:
	xor a0, t0, t1
	ret
.Lfunc_end0:
	.size	test, .-test
