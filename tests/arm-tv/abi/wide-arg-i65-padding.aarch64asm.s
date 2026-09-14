	.file	"wide-arg-i65-padding.body.ll"
	.text
	.globl	f                               // -- Begin function f
	.p2align	2
	.type	f,@function
f:                                      // @f
	.cfi_startproc
// %bb.0:
	and	x0, x1, #0x1
	ret
.Lfunc_end0:
	.size	f, .Lfunc_end0-f
	.cfi_endproc
                                        // -- End function
	.section	".note.GNU-stack","",@progbits
