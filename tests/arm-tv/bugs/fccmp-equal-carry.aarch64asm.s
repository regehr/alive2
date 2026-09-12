        .text
        .globl fccmp_equal_carry
        .p2align 2
        .type fccmp_equal_carry,@function
fccmp_equal_carry:
        fmov d0, #1.0
        cmp wzr, wzr
        fccmp d0, d0, #0, eq
        mrs x0, nzcv
        ret
        .size fccmp_equal_carry, .-fccmp_equal_carry
