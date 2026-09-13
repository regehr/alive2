; CHECK: 1 incorrect transformations

; The 63 bits above bit 64 of an i65 are unspecified, so the callee must not
; assume they are clean: this assembly does, and must be rejected.
define i64 @f(i65 %x) {
  %s = lshr i65 %x, 64
  %t = trunc i65 %s to i64
  ret i64 %t
}
