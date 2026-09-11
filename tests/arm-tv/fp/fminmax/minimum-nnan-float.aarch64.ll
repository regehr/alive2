; Default FPCR; symbolic inputs cover NaNs, infinities, and signed zeros.
define float @minimum_nnan_float(float noundef %a, float noundef %b) {
  %r = call nnan float @llvm.minimum.f32(float %a, float %b)
  ret float %r
}
