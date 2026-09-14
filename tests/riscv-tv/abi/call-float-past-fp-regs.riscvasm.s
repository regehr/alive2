	.text
	.file	1 "foo.ll"
	.globl	test
	.type	test,@function
test:
	addi sp, sp, -16
	sd ra, 8(sp)
	fli.s fa0, 1.0
	fli.s fa1, 1.0
	fli.s fa2, 1.0
	fli.s fa3, 1.0
	fli.s fa4, 1.0
	fli.s fa5, 1.0
	fli.s fa6, 1.0
	fli.s fa7, 1.0
	li a0, 1065353216
	.loc 1 1 0
	call consume
	ld ra, 8(sp)
	addi sp, sp, 16
	ret
.Lfunc_end0:
	.size	test, .-test
