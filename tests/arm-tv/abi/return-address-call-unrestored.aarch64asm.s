	.text
	.file	1 "foo.ll"
	.globl	test
	.type	test,@function
test:
	.loc 1 1 0
	bl callee
	ret
	.size	test, .-test
