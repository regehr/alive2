; Permit either sign for mixed-sign zeros while retaining NaN handling.
define float @maximumnum_nsz_float(float noundef %a, float noundef %b) {
  %r = call nsz float @llvm.maximumnum.f32(float %a, float %b)
  ret float %r
}
