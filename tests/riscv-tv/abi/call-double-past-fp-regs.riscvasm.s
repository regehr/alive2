	.text
	.file	1 "foo.ll"
	.globl	test
	.type	test,@function
test:
	addi sp, sp, -16
	sd ra, 8(sp)
	li a0, 1
	fli.d fa0, 1.0
	fli.d fa1, 1.0
	slli a0, a0, 62
	fli.d fa2, 1.0
	fli.d fa3, 1.0
	fli.d fa4, 1.0
	fli.d fa5, 1.0
	fli.d fa6, 1.0
	fli.d fa7, 1.0
	.loc 1 1 0
	call consume
	ld ra, 8(sp)
	addi sp, sp, 16
	ret
.Lfunc_end0:
	.size	test, .-test
