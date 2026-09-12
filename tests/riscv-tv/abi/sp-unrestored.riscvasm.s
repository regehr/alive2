	.text
	.globl	sp_unrestored
	.type	sp_unrestored,@function
sp_unrestored:
	addi	sp, sp, -16
	beqz	a0, .Lunrestored
	addi	sp, sp, 16
	li	a0, 42
	ret
.Lunrestored:
	li	a0, 42
	ret
.Lfunc_end0:
	.size	sp_unrestored, .-sp_unrestored
