	.text
	.file	1 "foo.ll"
	.globl	test
	.type	test,@function
test:
	stp x19, x30, [sp, #-16]!
	.loc 1 1 0
	bl clobber
	mov x19, x1
	.loc 1 2 0
	bl clobber
	eor x0, x19, x1
	ldp x19, x30, [sp], #16
	ret
	.size	test, .-test
