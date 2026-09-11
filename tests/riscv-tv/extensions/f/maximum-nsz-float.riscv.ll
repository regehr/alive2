; Permit either sign for mixed-sign zeros while retaining NaN handling.
define float @maximum_nsz_float(float noundef %a, float noundef %b) {
  %r = call nsz float @llvm.maximum.f32(float %a, float %b)
  ret float %r
}
