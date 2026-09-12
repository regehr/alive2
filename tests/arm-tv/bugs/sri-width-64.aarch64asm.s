        .text
        .globl sri_width_64
        .p2align 2
        .type sri_width_64,@function
sri_width_64:
        mov x0, #2
        fmov d0, x0
        mov x0, #1
        fmov d1, x0
        sri v0.2d, v1.2d, #64
        fmov x0, d0
        ret
        .size sri_width_64, .-sri_width_64
