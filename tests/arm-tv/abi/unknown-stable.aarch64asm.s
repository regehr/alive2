	.text
	.file	1 "foo.ll"
	.globl	test
	.type	test,@function
test:
	eor x0, x9, x9
	ret
.Lfunc_end0:
	.size	test, .-test
