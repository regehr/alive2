	.text
	.file	1 "foo.ll"
	.globl	test
	.type	test,@function
test:
	str x30, [sp, #-16]!
	mov x9, #42
	.loc 1 1 0
	bl clobber
	mov x0, x9
	ldr x30, [sp], #16
	ret
.Lfunc_end0:
	.size	test, .-test
