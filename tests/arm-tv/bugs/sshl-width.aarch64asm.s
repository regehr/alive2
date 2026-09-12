        .text
        .globl sshl_width
        .p2align 2
        .type sshl_width,@function
sshl_width:
        movi v0.4s, #1
        movi v1.4s, #32
        sshl v0.4s, v0.4s, v1.4s
        umov w0, v0.s[0]
        ret
        .size sshl_width, .-sshl_width
