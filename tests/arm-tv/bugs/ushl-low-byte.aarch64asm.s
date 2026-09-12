        .text
        .globl ushl_low_byte
        .p2align 2
        .type ushl_low_byte,@function
ushl_low_byte:
        movi v0.4s, #2
        movi v1.4s, #255
        ushl v0.4s, v0.4s, v1.4s
        umov w0, v0.s[0]
        ret
        .size ushl_low_byte, .-ushl_low_byte
