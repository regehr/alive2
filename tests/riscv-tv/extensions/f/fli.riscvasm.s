	.attribute	4, 16
	.attribute	5, "rv64i2p1_f2p2_d2p2_zfa1p0_zfh1p0"
	.text
	.globl	fli
	.type	fli,@function
fli:
	fli.s	ft0, min
	fmv.x.w	a0, ft0
	fli.s	ft0, inf
	fmv.x.w	a1, ft0
	xor	a0, a0, a1
	fli.s	ft0, nan
	fmv.x.w	a1, ft0
	xor	a0, a0, a1
	fli.h	ft0, min
	fmv.x.h	a1, ft0
	xor	a0, a0, a1
	fli.h	ft0, 65536.0
	fmv.x.h	a1, ft0
	xor	a0, a0, a1
	fli.d	ft0, min
	fmv.x.d	a1, ft0
	xor	a0, a0, a1
	ret
	.size	fli, .-fli
