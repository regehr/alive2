; NaN inputs produce poison; infinities and signed zeros remain unrestricted.
define double @minimum_nnan_double(double noundef %a, double noundef %b) {
  %r = call nnan double @llvm.minimum.f64(double %a, double %b)
  ret double %r
}
