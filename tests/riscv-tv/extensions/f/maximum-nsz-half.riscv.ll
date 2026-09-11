; Permit either sign for mixed-sign zeros while retaining NaN handling.
define half @maximum_nsz_half(half noundef %a, half noundef %b) {
  %r = call nsz half @llvm.maximum.f16(half %a, half %b)
  ret half %r
}
