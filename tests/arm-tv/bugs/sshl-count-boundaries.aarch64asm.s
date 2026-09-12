        .text
        .globl sshl_count_boundaries
        .p2align 2
        .type sshl_count_boundaries,@function
sshl_count_boundaries:
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
        movz x1, #63360
        movk x1, #63992, lsl #16
        movk x1, #255, lsl #32
        movk x1, #1793, lsl #48
        fmov d2, x1
        movz x2, #2312
        movk x2, #32895, lsl #16
        movk x2, #63735, lsl #32
        movk x2, #65529, lsl #48
        ins v2.d[1], x2
        movi v0.16b, #255
        sshl v0.16b, v1.16b, v2.16b
        fmov x1, d0
        movz x3, #255
        movk x3, #255, lsl #16
        movk x3, #32449, lsl #32
        movk x3, #6, lsl #48
        eor x1, x1, x3
        ext v0.16b, v0.16b, v0.16b, #8
        fmov x2, d0
        movz x3, #0
        movk x3, #255, lsl #32
        movk x3, #16383, lsl #48
        eor x2, x2, x3
        orr x0, x1, x2
        ret
        .size sshl_count_boundaries, .-sshl_count_boundaries
