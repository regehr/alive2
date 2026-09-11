; Default FPCR; symbolic inputs cover NaNs, infinities, and signed zeros.
define float @minimumnum_nnan_float(float noundef %a, float noundef %b) {
  %r = call nnan float @llvm.minimumnum.f32(float %a, float %b)
  ret float %r
}
