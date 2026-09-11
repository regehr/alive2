; NaN inputs produce poison; infinities and signed zeros remain unrestricted.
define double @minnum_nnan_double(double noundef %a, double noundef %b) {
  %r = call nnan double @llvm.minnum.f64(double %a, double %b)
  ret double %r
}
