	.text
	.globl	test
	.type	test,@function
test:
	and x30, x30, #-4
	ret
	.size	test, .-test
