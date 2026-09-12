        .text
        .globl ushr_width_64
        .p2align 2
        .type ushr_width_64,@function
ushr_width_64:
        mov x0, #1
        dup v0.2d, x0
        ushr v0.2d, v0.2d, #64
        fmov x0, d0
        ret
        .size ushr_width_64, .-ushr_width_64
