        .text
        .globl fcmp_equal_carry
        .p2align 2
        .type fcmp_equal_carry,@function
fcmp_equal_carry:
        fmov d0, #1.0
        fcmp d0, d0
        mrs x0, nzcv
        ret
        .size fcmp_equal_carry, .-fcmp_equal_carry
