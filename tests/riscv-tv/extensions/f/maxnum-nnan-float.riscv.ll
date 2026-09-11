; NaN inputs produce poison; infinities and signed zeros remain unrestricted.
define float @maxnum_nnan_float(float noundef %a, float noundef %b) {
  %r = call nnan float @llvm.maxnum.f32(float %a, float %b)
  ret float %r
}
