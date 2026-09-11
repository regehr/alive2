; Symbolic operands cover finite values, infinities, NaNs, and signed zeros.
define double @maximumnum_double(double noundef %a, double noundef %b) {
  %r = call double @llvm.maximumnum.f64(double %a, double %b)
  ret double %r
}
