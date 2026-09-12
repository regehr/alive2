        .text
        .globl ldpsw_preindex
        .p2align 2
        .type ldpsw_preindex,@function
ldpsw_preindex:
        sub sp, sp, #16
        mov w0, #1
        str w0, [sp]
        mov w0, #2
        str w0, [sp, #4]
        mov w0, #3
        str w0, [sp, #8]
        mov w0, #4
        str w0, [sp, #12]
        mov x2, sp
        ldpsw x0, x1, [x2, #8]!
        add sp, sp, #16
        ret
        .size ldpsw_preindex, .-ldpsw_preindex
