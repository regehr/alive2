	.text
	.file	1 "foo.ll"
	.globl	test
	.type	test,@function
test:
	addi sp, sp, -16
	sd ra, 8(sp)
	lui t0, 262144
	fli.s fa0, 1.0
	fli.s fa1, 1.0
	fli.s fa2, 1.0
	fli.s fa3, 1.0
	fli.s fa4, 1.0
	fli.s fa5, 1.0
	fli.s fa6, 1.0
	fli.s fa7, 1.0
	li a0, 0
	li a1, 0
	li a2, 0
	li a3, 0
	li a4, 0
	li a5, 0
	li a6, 0
	li a7, 0
	sw t0, 4(sp)
	.loc 1 1 0
	call consume
	ld ra, 8(sp)
	addi sp, sp, 16
	ret
.Lfunc_end0:
	.size	test, .-test
