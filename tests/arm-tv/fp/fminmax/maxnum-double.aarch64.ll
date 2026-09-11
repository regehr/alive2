; Default FPCR; symbolic inputs cover NaNs, infinities, and signed zeros.
define double @maxnum_double(double noundef %a, double noundef %b) {
  %r = call double @llvm.maxnum.f64(double %a, double %b)
  ret double %r
}
