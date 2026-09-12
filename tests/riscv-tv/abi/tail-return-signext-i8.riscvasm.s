	.text
	.file	1 "foo.ll"
	.globl	test
	.type	test,@function
test:
	.loc 1 1 0
	tail callee
.Lfunc_end0:
	.size	test, .-test
