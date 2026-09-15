; ERROR: Couldn't prove the correctness of the transformation

; The rewrite-based flags have no local meaning, so -- as for fptrunc & fpext
; -- Alive2 models them by approximating the result. Dropping one is then not
; provable, rather than incorrect.

define half @src(i8 %x) {
  %r = sitofp reassoc i8 %x to half
  ret half %r
}

define half @tgt(i8 %x) {
  %r = sitofp i8 %x to half
  ret half %r
}
