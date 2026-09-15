; An int -> fp conversion whose result overflows the destination format is
; poison under ninf, so dropping the flag is a refinement.

define half @src(i64 %x) {
  %r = sitofp ninf i64 %x to half
  ret half %r
}

define half @tgt(i64 %x) {
  %r = sitofp i64 %x to half
  ret half %r
}
