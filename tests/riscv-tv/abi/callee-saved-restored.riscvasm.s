	.text
	.file	1 "foo.ll"
	.globl	test
	.type	test,@function
test:
	addi sp, sp, -80
	sd ra, 72(sp)
	sd s0, 0(sp)
	sd s1, 8(sp)
	sd s2, 16(sp)
	sd s11, 24(sp)
	fsd fs0, 32(sp)
	fsd fs1, 40(sp)
	fsd fs2, 48(sp)
	fsd fs11, 56(sp)
	li s0, 41
	li s1, 0
	li s2, 0
	li s11, 0
	fli.d fs0, 1.0
	fmv.d.x fs1, zero
	fmv.d.x fs2, zero
	fmv.d.x fs11, zero
	.loc 1 1 0
	call clobber
	fcvt.l.d a0, fs0, rtz
	add a0, a0, s0
	fld fs11, 56(sp)
	fld fs2, 48(sp)
	fld fs1, 40(sp)
	fld fs0, 32(sp)
	ld s11, 24(sp)
	ld s2, 16(sp)
	ld s1, 8(sp)
	ld s0, 0(sp)
	ld ra, 72(sp)
	addi sp, sp, 80
	ret
.Lfunc_end0:
	.size	test, .-test
