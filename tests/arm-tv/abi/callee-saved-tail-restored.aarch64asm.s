	.text
	.file	1 "foo.ll"
	.globl	test
	.type	test,@function
test:
	stp x19, x30, [sp, #-16]!
	mov x19, #0
	mov x30, #0
	ldp x19, x30, [sp], #16
	.loc 1 1 0
	b callee
	.size	test, .-test
