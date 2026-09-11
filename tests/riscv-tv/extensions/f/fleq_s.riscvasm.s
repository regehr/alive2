	.attribute	4, 16
	.attribute	5, "rv64i2p1_f2p2_d2p2_zfa1p0"
	.text
	.globl	fleq_s
	.type	fleq_s,@function
fleq_s:
	fleq.s	a0, fa0, fa1
	ret
	.size	fleq_s, .-fleq_s
