	.text
	.file	1 "foo.ll"
	.globl	test
	.type	test,@function
test:
	addi sp, sp, -32
	sd ra, 24(sp)
	sd s0, 16(sp)
	sd s1, 8(sp)
	li s1, 2
.Lloop:
	.loc 1 2 0
	call clobber
	addi s1, s1, -1
	beqz s1, .Lexit
	mv s0, t0
	j .Lloop
.Lexit:
	xor a0, s0, t0
	ld s1, 8(sp)
	ld s0, 16(sp)
	ld ra, 24(sp)
	addi sp, sp, 32
	ret
.Lfunc_end0:
	.size	test, .-test
