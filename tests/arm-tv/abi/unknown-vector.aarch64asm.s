	.text
	.file	1 "foo.ll"
	.globl	test
	.type	test,@function
test:
	umov x0, v0.d[1]
	ret
.Lfunc_end0:
	.size	test, .-test
