	.text
	.globl	test
	.type	test,@function
test:
	xori	ra, ra, 2
	ret
.Lfunc_end0:
	.size	test, .-test
