        .text
        .globl sshl_right_width
        .p2align 2
        .type sshl_right_width,@function
sshl_right_width:
        mov w0, #0x80000000
        dup v0.4s, w0
        mov w0, #-32
        dup v1.4s, w0
        sshl v0.4s, v0.4s, v1.4s
        umov w0, v0.s[0]
        ret
        .size sshl_right_width, .-sshl_right_width
