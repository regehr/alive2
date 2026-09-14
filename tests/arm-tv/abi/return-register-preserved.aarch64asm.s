	.text
	.globl	test
	.type	test,@function
test:
	mov x9, x30
	mov x30, #0
	ret x9
	.size	test, .-test
