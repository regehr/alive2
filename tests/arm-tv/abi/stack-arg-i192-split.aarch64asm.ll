; CHECK: 1 correct transformations

; A wide scalar really can straddle the boundary: two limbs in x6 and x7,
; the third on the stack. AAPCS64 does not do this for composites, but the
; backend does it for an integer.
define i64 @f(i64 %a,i64 %b,i64 %c,i64 %d,i64 %e,i64 %g, i192 %w) {
  %s = lshr i192 %w, 128
  %t = trunc i192 %s to i64
  ret i64 %t
}
