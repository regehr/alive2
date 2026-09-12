	.text
	.file	1 "foo.ll"
	.globl	test
	.type	test,@function
test:
	.loc	1 1 0
	call	callee
	ret
.Lfunc_end0:
	.size	test, .-test
