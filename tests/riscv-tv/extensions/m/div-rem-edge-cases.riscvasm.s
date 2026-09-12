        .text
        .globl div_rem_edge_cases
        .p2align 2
        .type div_rem_edge_cases,@function
div_rem_edge_cases:
        # Accumulate mismatches with OR so they cannot cancel each other.
        li a0, 0

        # Signed overflow at XLEN=64.
        li t1, 0x8000000000000000
        li t2, -1
        div t0, t1, t2
        xor t0, t0, t1
        or a0, a0, t0
        rem t0, t1, t2
        or a0, a0, t0

        # Division/remainder by zero preserves the specified ISA results.
        div t0, t1, zero
        not t0, t0
        or a0, a0, t0
        divu t0, t1, zero
        not t0, t0
        or a0, a0, t0
        rem t0, t1, zero
        xor t0, t0, t1
        or a0, a0, t0
        remu t0, t1, zero
        xor t0, t0, t1
        or a0, a0, t0

        # Include zero / zero.
        div t0, zero, zero
        not t0, t0
        or a0, a0, t0
        divu t0, zero, zero
        not t0, t0
        or a0, a0, t0
        rem t0, zero, zero
        or a0, a0, t0
        remu t0, zero, zero
        or a0, a0, t0

        # Word operations ignore deliberately noncanonical upper bits.
        li t1, 0x1234567880000000
        li t2, 0x12345678ffffffff
        li t3, -2147483648
        divw t0, t1, t2
        xor t0, t0, t3
        or a0, a0, t0
        remw t0, t1, t2
        or a0, a0, t0

        # Only the low word of this nonzero register is zero.
        li t2, 0x1234567800000000
        divw t0, t1, t2
        not t0, t0
        or a0, a0, t0
        divuw t0, t1, t2
        not t0, t0
        or a0, a0, t0
        remw t0, t1, t2
        xor t0, t0, t3
        or a0, a0, t0
        remuw t0, t1, t2
        xor t0, t0, t3
        or a0, a0, t0

        # Unsigned word operations sign-extend even ordinary results.
        li t2, 0x1234567800000001
        divuw t0, t1, t2
        xor t0, t0, t3
        or a0, a0, t0
        li t2, 0x12345678ffffffff
        remuw t0, t1, t2
        xor t0, t0, t3
        or a0, a0, t0
        ret
.Lfunc_end0:
        .size div_rem_edge_cases, .Lfunc_end0-div_rem_edge_cases
