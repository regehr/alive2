	.file	"big.ll"
	.text
	.globl	f                               // -- Begin function f
	.p2align	2
	.type	f,@function
f:                                      // @f
	.cfi_startproc
// %bb.0:
	ldp	q1, q0, [x0, #32]
	ldr	x9, [x0, #64]
	ldp	q2, q3, [x0]
	str	x9, [x8, #64]
	stp	q1, q0, [x8, #32]
	stp	q2, q3, [x8]
	ret
.Lfunc_end0:
	.size	f, .Lfunc_end0-f
	.cfi_endproc
                                        // -- End function
	.section	".note.GNU-stack","",@progbits
