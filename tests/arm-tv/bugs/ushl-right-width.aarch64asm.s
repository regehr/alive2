        .text
        .globl ushl_right_width
        .p2align 2
        .type ushl_right_width,@function
ushl_right_width:
        movi v0.4s, #1
        mov w0, #-32
        dup v1.4s, w0
        ushl v0.4s, v0.4s, v1.4s
        umov w0, v0.s[0]
        ret
        .size ushl_right_width, .-ushl_right_width
