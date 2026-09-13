	.text
	.file	1 "foo.ll"
	.globl	test
	.type	test,@function
test:
	str x30, [sp, #-16]!
	mov x1, #42
	.loc 1 1 0
	blr x0
	mov x0, x1
	ldr x30, [sp], #16
	ret
	.size	test, .-test
