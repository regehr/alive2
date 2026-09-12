	.text
	.globl	jal_link
	.type	jal_link,@function
jal_link:
	li	t0, 0
	jal	t0, .Ltarget
.Ltarget:
	mv	a0, t0
	ret
.Lfunc_end0:
	.size	jal_link, .-jal_link
