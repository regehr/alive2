        .text
        .globl ushr_width
        .p2align 2
        .type ushr_width,@function
ushr_width:
        movi v0.4s, #1
        ushr v0.4s, v0.4s, #32
        umov w0, v0.s[0]
        ret
        .size ushr_width, .-ushr_width
