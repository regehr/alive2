        .text
        .globl sdiv_overflow_32
        .p2align 2
        .type sdiv_overflow_32,@function
sdiv_overflow_32:
        mov w1, #0x80000000
        mov w2, #-1
        sdiv w0, w1, w2
        ret
.Lfunc_end0:
        .size sdiv_overflow_32, .Lfunc_end0-sdiv_overflow_32
