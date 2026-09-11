; Permit either sign for mixed-sign zeros while retaining NaN handling.
define float @minnum_nsz_float(float noundef %a, float noundef %b) {
  %r = call nsz float @llvm.minnum.f32(float %a, float %b)
  ret float %r
}
