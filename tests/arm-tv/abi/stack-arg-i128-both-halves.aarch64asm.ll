; CHECK: 1 correct transformations

; With seven integer registers already spoken for, AAPCS64's even-register
; rule cannot place the first limb, so it burns x7 and the whole i128 goes
; to the stack at 16-byte alignment.
define i64 @f(i64 %a,i64 %b,i64 %c,i64 %d,i64 %e,i64 %g,i64 %h, i128 %w) {
  %s = lshr i128 %w, 64
  %t = trunc i128 %s to i64
  ret i64 %t
}
