; Default FPCR; symbolic inputs cover NaNs, infinities, and signed zeros.
define double @minimumnum_double(double noundef %a, double noundef %b) {
  %r = call double @llvm.minimumnum.f64(double %a, double %b)
  ret double %r
}
