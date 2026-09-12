        .text
        .globl usra_width
        .p2align 2
        .type usra_width,@function
usra_width:
        movi v0.4s, #5
        movi v1.4s, #1
        usra v0.4s, v1.4s, #32
        umov w0, v0.s[0]
        ret
        .size usra_width, .-usra_width
