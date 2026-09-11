; Default FPCR; symbolic inputs cover NaNs, infinities, and signed zeros.
define float @maxnum_nnan_float(float noundef %a, float noundef %b) {
  %r = call nnan float @llvm.maxnum.f32(float %a, float %b)
  ret float %r
}
