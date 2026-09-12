	.text
	.file	1 "foo.ll"
	.globl	test
	.type	test,@function
test:
	fmv.d.x fs0, zero
	ret
.Lfunc_end0:
	.size	test, .-test
