	.text
	.file	1 "foo.ll"
	.globl	test
	.type	test,@function
test:
	mov x0, #0x1ffffffff
	ret
	.size	test, .-test
