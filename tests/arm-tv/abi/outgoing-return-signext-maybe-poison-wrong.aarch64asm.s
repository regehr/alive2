	.text
	.file	1 "foo.ll"
	.globl	test
	.type	test,@function
test:
	tbz w0, #0, .Ldefined
	mov w0, #255
	ret
.Ldefined:
	mov w0, #255
	ret
	.size	test, .-test
