        .text
        .globl ldpsw_preindex_negative
        .p2align 2
        .type ldpsw_preindex_negative,@function
ldpsw_preindex_negative:
        sub sp, sp, #16
        mov w0, #1
        str w0, [sp]
        mov w0, #-2
        str w0, [sp, #4]
        mov w0, #3
        str w0, [sp, #8]
        mov w0, #4
        str w0, [sp, #12]
        add x2, sp, #8
        ldpsw x0, x1, [x2, #-8]!
        eor x0, x0, #1
        add x1, x1, #2
        orr x0, x0, x1
        mov x3, sp
        sub x2, x2, x3
        orr x0, x0, x2
        add sp, sp, #16
        ret
        .size ldpsw_preindex_negative, .-ldpsw_preindex_negative
