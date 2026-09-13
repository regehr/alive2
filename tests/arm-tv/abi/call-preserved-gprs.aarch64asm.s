	.text
	.file	1 "foo.ll"
	.globl	test
	.type	test,@function
test:
	stp x19, x28, [sp, #-32]!
	str x30, [sp, #16]
	mov x19, x0
	mov x28, x1
	.loc 1 1 0
	bl clobber
	eor x0, x19, x28
	ldr x30, [sp, #16]
	ldp x19, x28, [sp], #32
	ret
	.size	test, .-test
