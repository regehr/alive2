; Permit either sign for mixed-sign zeros while retaining NaN handling.
define float @minimumnum_nsz_float(float noundef %a, float noundef %b) {
  %r = call nsz float @llvm.minimumnum.f32(float %a, float %b)
  ret float %r
}
