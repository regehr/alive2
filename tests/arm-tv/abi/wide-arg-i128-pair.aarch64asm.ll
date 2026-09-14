; CHECK: 1 correct transformations

; AAPCS64 rounds the next register up to an even number before a value
; that occupies more than one, so the i128 lands in x2/x3 and x1 is burned.
define i64 @f(i64 %p, i128 %x) {
  %s = lshr i128 %x, 64
  %t = trunc i128 %s to i64
  ret i64 %t
}
