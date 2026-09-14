; CHECK: 1 correct transformations

; RV64 has no even-register rule, so unlike AArch64 the i128 follows the
; i64 straight into a1/a2.
define i64 @f(i64 %p, i128 %x) {
  %s = lshr i128 %x, 64
  %t = trunc i128 %s to i64
  ret i64 %t
}
