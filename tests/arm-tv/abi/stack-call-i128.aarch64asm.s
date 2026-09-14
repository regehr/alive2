	.file	"stack-call-i128.body.ll"
	.text
	.globl	f                               // -- Begin function f
	.p2align	2
	.type	f,@function
f:                                      // @f
	.cfi_startproc
// %bb.0:
	sub	sp, sp, #32
	stp	x3, x30, [sp, #8]               // 8-byte Folded Spill
	.cfi_def_cfa_offset 32
	.cfi_offset w30, -16
	mov	x8, x2
	mov	x1, x0
	mov	x2, x0
	mov	x3, x0
	mov	x4, x0
	mov	x5, x0
	mov	x6, x0
	str	x8, [sp]
	bl	g
	ldr	x30, [sp, #16]                  // 8-byte Reload
	add	sp, sp, #32
	ret
.Lfunc_end0:
	.size	f, .Lfunc_end0-f
	.cfi_endproc
                                        // -- End function
	.section	".note.GNU-stack","",@progbits
