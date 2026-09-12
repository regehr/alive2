	.text
	.file	1 "foo.ll"
	.globl	test
	.type	test,@function
test:
	addi sp, sp, -16
	sd ra, 8(sp)
	fli.s fa0, 1.0
	li a0, 255
	li a1, 0xffffffff
	.loc 1 1 0
	call consume
	ld ra, 8(sp)
	addi sp, sp, 16
	ret
.Lfunc_end0:
	.size	test, .-test
