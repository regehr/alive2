	.text
	.file	1 "foo.ll"
	.globl	test
	.type	test,@function
test:
	addi sp, sp, -16
	sd ra, 8(sp)
	.loc 1 1 0
	call __backend_tv_unknown_i64
	li a0, 0
	ld ra, 8(sp)
	addi sp, sp, 16
	ret
.Lfunc_end0:
	.size	test, .-test
