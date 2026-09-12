        .text
        .globl sri_width_scalar
        .p2align 2
        .type sri_width_scalar,@function
sri_width_scalar:
        movz x1, #23205
        movk x1, #42330, lsl #16
        movk x1, #38460, lsl #32
        movk x1, #50025, lsl #48
        fmov d0, x1
        movz x2, #42330
        movk x2, #23205, lsl #16
        movk x2, #38505, lsl #32
        movk x2, #15555, lsl #48
        ins v0.d[1], x2
        movz x1, #42330
        movk x1, #23205, lsl #16
        movk x1, #27075, lsl #32
        movk x1, #15510, lsl #48
        fmov d1, x1
        movz x2, #23205
        movk x2, #42330, lsl #16
        movk x2, #27030, lsl #32
        movk x2, #49980, lsl #48
        ins v1.d[1], x2
        sri d0, d1, #64
        fmov x1, d0
        movz x3, #23205
        movk x3, #42330, lsl #16
        movk x3, #38460, lsl #32
        movk x3, #50025, lsl #48
        eor x1, x1, x3
        ext v0.16b, v0.16b, v0.16b, #8
        fmov x2, d0
        movz x3, #0
        eor x2, x2, x3
        orr x0, x1, x2
        ret
        .size sri_width_scalar, .-sri_width_scalar
