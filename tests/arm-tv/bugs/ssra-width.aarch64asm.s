        .text
        .globl ssra_width
        .p2align 2
        .type ssra_width,@function
ssra_width:
        movi v0.4s, #5
        mov w0, #0x80000000
        dup v1.4s, w0
        ssra v0.4s, v1.4s, #32
        umov w0, v0.s[0]
        ret
        .size ssra_width, .-ssra_width
