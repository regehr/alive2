	.text
	.file	1 "foo.ll"
	.globl	test
	.type	test,@function
test:
	mv a0, t0
	ret
.Lfunc_end0:
	.size	test, .-test
