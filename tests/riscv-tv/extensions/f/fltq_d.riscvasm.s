	.attribute	4, 16
	.attribute	5, "rv64i2p1_f2p2_d2p2_zfa1p0"
	.text
	.globl	fltq_d
	.type	fltq_d,@function
fltq_d:
	fltq.d	a0, fa0, fa1
	ret
	.size	fltq_d, .-fltq_d
