        .text
        .globl sshl_low_byte
        .p2align 2
        .type sshl_low_byte,@function
sshl_low_byte:
        mov w0, #-4
        dup v0.4s, w0
        movi v1.4s, #255
        sshl v0.4s, v0.4s, v1.4s
        umov w0, v0.s[0]
        ret
        .size sshl_low_byte, .-sshl_low_byte
