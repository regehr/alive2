	.option	rvc
	.text
	.globl	test
	.type	test,@function
test:
	xori	ra, ra, 1
	c.jr	ra
.Lfunc_end0:
	.size	test, .-test
