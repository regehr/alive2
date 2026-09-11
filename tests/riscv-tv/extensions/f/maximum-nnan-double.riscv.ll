; NaN inputs produce poison; infinities and signed zeros remain unrestricted.
define double @maximum_nnan_double(double noundef %a, double noundef %b) {
  %r = call nnan double @llvm.maximum.f64(double %a, double %b)
  ret double %r
}
