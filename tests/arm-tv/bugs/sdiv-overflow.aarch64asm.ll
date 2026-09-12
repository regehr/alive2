; AArch64 SDIV returns INT_MIN for INT_MIN / -1.
;
; Specify the expected result directly: LLVM sdiv would be undefined for
; these operands and therefore could not detect the incorrect lifting.

define i64 @sdiv_overflow() {
  ret i64 -9223372036854775808
}
