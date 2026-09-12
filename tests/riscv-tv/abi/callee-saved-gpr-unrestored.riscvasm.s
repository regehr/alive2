	.text
	.file	1 "foo.ll"
	.globl	test
	.type	test,@function
test:
	beqz a0, .Lunrestored
	li a0, 42
	ret
.Lunrestored:
	li s11, 0
	li a0, 42
	ret
.Lfunc_end0:
	.size	test, .-test
