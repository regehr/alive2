	.text
	.globl	test
	.type	test,@function
test:
	ldr w0, [sp]
	ret
	.size	test, .-test
