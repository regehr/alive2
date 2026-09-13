	.text
	.globl	test
	.type	test,@function
test:
	ldr x0, [sp]
	ret
	.size	test, .-test
