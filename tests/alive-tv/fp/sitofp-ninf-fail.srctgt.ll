; ERROR: Target is more poisonous than source

; Adding ninf is not a refinement: 2^43 is representable in i64 but overflows
; to +oo in half, which ninf turns into poison.

define half @src(i64 %x) {
  %r = sitofp i64 %x to half
  ret half %r
}

define half @tgt(i64 %x) {
  %r = sitofp ninf i64 %x to half
  ret half %r
}
