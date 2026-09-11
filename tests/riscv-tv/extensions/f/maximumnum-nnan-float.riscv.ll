; NaN inputs produce poison; infinities and signed zeros remain unrestricted.
define float @maximumnum_nnan_float(float noundef %a, float noundef %b) {
  %r = call nnan float @llvm.maximumnum.f32(float %a, float %b)
  ret float %r
}
