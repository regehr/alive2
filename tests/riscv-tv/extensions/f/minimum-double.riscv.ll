; Symbolic operands cover finite values, infinities, NaNs, and signed zeros.
define double @minimum_double(double noundef %a, double noundef %b) {
  %r = call double @llvm.minimum.f64(double %a, double %b)
  ret double %r
}
