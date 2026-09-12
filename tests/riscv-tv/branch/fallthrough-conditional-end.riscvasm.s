	.option	norvc
	.text
	.globl	fallthrough_conditional_end
	.type	fallthrough_conditional_end,@function
fallthrough_conditional_end:
	j	.Ltest
.Lreturn:
	li	a0, 42
	ret
.Ltest:
	bnez	a0, .Lreturn
.Lfunc_end0:
	.size	fallthrough_conditional_end, .-fallthrough_conditional_end
