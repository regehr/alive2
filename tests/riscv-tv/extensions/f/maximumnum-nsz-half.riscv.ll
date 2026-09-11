; Permit either sign for mixed-sign zeros while retaining NaN handling.
define half @maximumnum_nsz_half(half noundef %a, half noundef %b) {
  %r = call nsz half @llvm.maximumnum.f16(half %a, half %b)
  ret half %r
}
