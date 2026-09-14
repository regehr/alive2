	.text
	.globl	test
	.type	test,@function
test:
	cbz x0, .Lreturn
	mov x19, #0
.Lreturn:
	ret
	.size	test, .-test
