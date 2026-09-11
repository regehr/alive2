	.attribute	4, 16
	.attribute	5, "rv64i2p1_f2p2_d2p2_zfa1p0"
	.text
	.globl	fltq_s
	.type	fltq_s,@function
fltq_s:
	fltq.s	a0, fa0, fa1
	ret
	.size	fltq_s, .-fltq_s
