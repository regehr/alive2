; ERROR: Target's return value is more undefined

; Adding nsz is not: sitofp of 0 is +0.0, but with nsz the sign of the zero
; result is non-deterministic.

define double @src(i32 %x) {
  %r = sitofp i32 %x to double
  ret double %r
}

define double @tgt(i32 %x) {
  %r = sitofp nsz i32 %x to double
  ret double %r
}
