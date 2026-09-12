	.text
	.file	1 "foo.ll"
	.globl	test
	.type	test,@function
test:
	addi sp, sp, -16
	sd s0, 0(sp)
	fsd fs0, 8(sp)
	li s0, 0
	fmv.d.x fs0, zero
	fld fs0, 8(sp)
	ld s0, 0(sp)
	addi sp, sp, 16
	.loc 1 1 0
	tail callee
.Lfunc_end0:
	.size	test, .-test
