	.attribute	4, 16
	.attribute	5, "rv64i2p1_f2p2_d2p2_zfa1p0"
	.text
	.globl	fminm_s
	.type	fminm_s,@function
fminm_s:
	fminm.s	ft0, fa0, fa1
	fmv.x.w	a0, ft0
	ret
	.size	fminm_s, .-fminm_s
