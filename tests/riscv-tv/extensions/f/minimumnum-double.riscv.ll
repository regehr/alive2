; Symbolic operands cover finite values, infinities, NaNs, and signed zeros.
define double @minimumnum_double(double noundef %a, double noundef %b) {
  %r = call double @llvm.minimumnum.f64(double %a, double %b)
  ret double %r
}
