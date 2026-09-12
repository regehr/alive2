        .text
        .globl sshr_width
        .p2align 2
        .type sshr_width,@function
sshr_width:
        mov w0, #0x80000000
        dup v0.4s, w0
        sshr v0.4s, v0.4s, #32
        umov w0, v0.s[0]
        ret
        .size sshr_width, .-sshr_width
