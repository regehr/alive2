        .text
        .globl sri_width
        .p2align 2
        .type sri_width,@function
sri_width:
        movi v0.4s, #0
        movi v1.4s, #1
        sri v0.4s, v1.4s, #32
        umov w0, v0.s[0]
        ret
        .size sri_width, .-sri_width
