	.text
	.file	1 "foo.ll"
	.globl	test
	.type	test,@function
test:
	li tp, 0
	ret
.Lfunc_end0:
	.size	test, .-test
