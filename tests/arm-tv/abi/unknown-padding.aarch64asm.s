	.text
	.file	1 "foo.ll"
	.globl	test
	.type	test,@function
test:
	lsr x0, x0, #13
	ret
.Lfunc_end0:
	.size	test, .-test
