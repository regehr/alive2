	.text
	.file	1 "foo.ll"
	.globl	test
	.type	test,@function
test:
	fmv.d.x fs11, zero
	.loc 1 1 0
	tail clobber
.Lfunc_end0:
	.size	test, .-test
