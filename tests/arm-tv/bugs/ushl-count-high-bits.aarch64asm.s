        .text
        .globl ushl_count_high_bits
        .p2align 2
        .type ushl_count_high_bits,@function
ushl_count_high_bits:
        movz x1, #65534
        movk x1, #32767, lsl #16
        movk x1, #3, lsl #32
        movk x1, #32768, lsl #48
        fmov d1, x1
        movz x2, #65534
        movk x2, #32767, lsl #16
        movk x2, #3, lsl #32
        movk x2, #32768, lsl #48
        ins v1.d[1], x2
        movz x1, #65280
        movk x1, #65535, lsl #16
        movk x1, #257, lsl #32
        fmov d2, x1
        movz x2, #65311
        movk x2, #65535, lsl #16
        movk x2, #288, lsl #32
        ins v2.d[1], x2
        movi v0.16b, #255
        ushl v0.4s, v1.4s, v2.4s
        fmov x1, d0
        movz x3, #65534
        movk x3, #32767, lsl #16
        movk x3, #6, lsl #32
        eor x1, x1, x3
        ext v0.16b, v0.16b, v0.16b, #8
        fmov x2, d0
        movz x3, #0
        eor x2, x2, x3
        orr x0, x1, x2
        ret
        .size ushl_count_high_bits, .-ushl_count_high_bits
