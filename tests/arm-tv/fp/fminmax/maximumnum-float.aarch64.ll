; Default FPCR; symbolic inputs cover NaNs, infinities, and signed zeros.
define float @maximumnum_float(float noundef %a, float noundef %b) {
  %r = call float @llvm.maximumnum.f32(float %a, float %b)
  ret float %r
}
