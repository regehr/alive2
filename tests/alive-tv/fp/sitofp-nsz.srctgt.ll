; Dropping nsz is a refinement: the source may return -0.0 for %x == 0, the
; target always returns +0.0.

define double @src(i32 %x) {
  %r = sitofp nsz i32 %x to double
  ret double %r
}

define double @tgt(i32 %x) {
  %r = sitofp i32 %x to double
  ret double %r
}
