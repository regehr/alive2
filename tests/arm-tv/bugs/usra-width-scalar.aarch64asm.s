.text
.globl usra_width_scalar
.p2align 2
.type usra_width_scalar,@function
usra_width_scalar:
        movz x1, #3
        movk x1, #32768, lsl #48
        fmov d1, x1
        movz x2, #65534
        movk x2, #65535, lsl #16
        movk x2, #65535, lsl #32
        movk x2, #32767, lsl #48
        ins v1.d[1], x2
        movi v0.16b, #255
        usra d0, d1, #64
        fmov x1, d0
        movz x3, #65535
        movk x3, #65535, lsl #16
        movk x3, #65535, lsl #32
        movk x3, #65535, lsl #48
        eor x1, x1, x3
        ext v0.16b, v0.16b, v0.16b, #8
        fmov x2, d0
        movz x3, #0
        eor x2, x2, x3
        orr x0, x1, x2
        ret
.size usra_width_scalar, .-usra_width_scalar
