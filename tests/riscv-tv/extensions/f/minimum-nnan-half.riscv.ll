; NaN inputs produce poison; infinities and signed zeros remain unrestricted.
define half @minimum_nnan_half(half noundef %a, half noundef %b) {
  %r = call nnan half @llvm.minimum.f16(half %a, half %b)
  ret half %r
}
