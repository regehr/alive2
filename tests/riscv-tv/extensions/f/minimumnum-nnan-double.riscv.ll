; NaN inputs produce poison; infinities and signed zeros remain unrestricted.
define double @minimumnum_nnan_double(double noundef %a, double noundef %b) {
  %r = call nnan double @llvm.minimumnum.f64(double %a, double %b)
  ret double %r
}
