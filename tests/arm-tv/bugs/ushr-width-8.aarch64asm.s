.text
.globl ushr_width_8
.p2align 2
.type ushr_width_8,@function
ushr_width_8:
        movz x1, #32387
        movk x1, #32387, lsl #16
        movk x1, #32387, lsl #32
        movk x1, #32387, lsl #48
        fmov d1, x1
        movz x2, #32387
        movk x2, #32387, lsl #16
        movk x2, #32387, lsl #32
        movk x2, #32387, lsl #48
        ins v1.d[1], x2
        movi v0.16b, #255
        ushr v0.16b, v1.16b, #8
        fmov x1, d0
        movz x3, #0
        eor x1, x1, x3
        ext v0.16b, v0.16b, v0.16b, #8
        fmov x2, d0
        movz x3, #0
        eor x2, x2, x3
        orr x0, x1, x2
        ret
.size ushr_width_8, .-ushr_width_8
