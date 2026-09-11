; Permit either sign for mixed-sign zeros while retaining NaN handling.
define half @maxnum_nsz_half(half noundef %a, half noundef %b) {
  %r = call nsz half @llvm.maxnum.f16(half %a, half %b)
  ret half %r
}
