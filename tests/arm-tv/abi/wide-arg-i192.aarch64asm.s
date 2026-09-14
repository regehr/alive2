	.file	"wide-arg-i192.body.ll"
	.text
	.globl	f                               // -- Begin function f
	.p2align	2
	.type	f,@function
f:                                      // @f
	.cfi_startproc
// %bb.0:
	mov	x0, x2
	ret
.Lfunc_end0:
	.size	f, .Lfunc_end0-f
	.cfi_endproc
                                        // -- End function
	.section	".note.GNU-stack","",@progbits
