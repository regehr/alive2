; AArch64 32-bit SDIV returns INT32_MIN for INT32_MIN / -1. Writing W0
; zero-extends that result into X0, which we observe through an i64 return.
;
; Specify the expected result directly to avoid LLVM sdiv's undefined
; behavior on signed overflow.

define i64 @sdiv_overflow_32() {
  ret i64 2147483648
}
