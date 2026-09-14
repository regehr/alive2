	.text
	.file	1 "foo.ll"
	.globl	test
	.type	test,@function
test:
	mov w0, #-1
	ret
	.size	test, .-test
