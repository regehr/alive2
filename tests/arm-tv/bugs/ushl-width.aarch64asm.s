        .text
        .globl ushl_width
        .p2align 2
        .type ushl_width,@function
ushl_width:
        movi v0.4s, #1
        movi v1.4s, #32
        ushl v0.4s, v0.4s, v1.4s
        umov w0, v0.s[0]
        ret
        .size ushl_width, .-ushl_width
