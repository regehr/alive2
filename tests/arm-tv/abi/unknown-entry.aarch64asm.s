	.text
	.file	1 "foo.ll"
	.globl	test
	.type	test,@function
test:
	mov x0, x9
	ret
.Lfunc_end0:
	.size	test, .-test
