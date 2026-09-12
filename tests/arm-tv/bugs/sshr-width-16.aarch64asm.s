.text
.globl sshr_width_16
.p2align 2
.type sshr_width_16,@function
sshr_width_16:
        movz x1, #32771
        movk x1, #32766, lsl #16
        movk x1, #32771, lsl #32
        movk x1, #32766, lsl #48
        fmov d1, x1
        movz x2, #32771
        movk x2, #32766, lsl #16
        movk x2, #32771, lsl #32
        movk x2, #32766, lsl #48
        ins v1.d[1], x2
        movi v0.16b, #255
        sshr v0.8h, v1.8h, #16
        fmov x1, d0
        movz x3, #65535
        movk x3, #65535, lsl #32
        eor x1, x1, x3
        ext v0.16b, v0.16b, v0.16b, #8
        fmov x2, d0
        movz x3, #65535
        movk x3, #65535, lsl #32
        eor x2, x2, x3
        orr x0, x1, x2
        ret
.size sshr_width_16, .-sshr_width_16
