; RISC-V floating-point computations produce the canonical NaN, while
; sign-injection instructions preserve NaN payloads.
define i64 @canonical_nan() {
  ret i64 32256 ; 0x7e00, the canonical half-precision NaN
}
