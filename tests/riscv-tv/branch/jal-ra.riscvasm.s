	.text
	.globl	jal_ra
	.type	jal_ra,@function
jal_ra:
	mv	t0, ra
	jal	.Ltarget
.Ltarget:
	mv	ra, t0
	li	a0, 42
	ret
.Lfunc_end0:
	.size	jal_ra, .-jal_ra
