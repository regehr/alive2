; NaN inputs produce poison; infinities and signed zeros remain unrestricted.
define half @maxnum_nnan_half(half noundef %a, half noundef %b) {
  %r = call nnan half @llvm.maxnum.f16(half %a, half %b)
  ret half %r
}
