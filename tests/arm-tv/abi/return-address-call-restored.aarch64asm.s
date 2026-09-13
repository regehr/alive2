	.text
	.file	1 "foo.ll"
	.globl	test
	.type	test,@function
test:
	stp x29, x30, [sp, #-16]!
	mov x29, sp
	.loc 1 1 0
	bl callee
	ldp x29, x30, [sp], #16
	ret
	.size	test, .-test
