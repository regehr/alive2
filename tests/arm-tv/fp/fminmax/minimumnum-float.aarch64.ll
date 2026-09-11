; Default FPCR; symbolic inputs cover NaNs, infinities, and signed zeros.
define float @minimumnum_float(float noundef %a, float noundef %b) {
  %r = call float @llvm.minimumnum.f32(float %a, float %b)
  ret float %r
}
