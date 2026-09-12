        .text
        .globl sdiv_overflow
        .p2align 2
        .type sdiv_overflow,@function
sdiv_overflow:
        mov x1, #0x8000000000000000
        mov x2, #-1
        sdiv x0, x1, x2
        ret
.Lfunc_end0:
        .size sdiv_overflow, .Lfunc_end0-sdiv_overflow
