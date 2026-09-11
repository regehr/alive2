; Default FPCR; symbolic inputs cover NaNs, infinities, and signed zeros.
define float @minnum_nsz_float(float noundef %a, float noundef %b) {
  %r = call nsz float @llvm.minnum.f32(float %a, float %b)
  ret float %r
}
