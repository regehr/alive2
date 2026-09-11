; Default FPCR; symbolic inputs cover NaNs, infinities, and signed zeros.
define double @maximum_nsz_double(double noundef %a, double noundef %b) {
  %r = call nsz double @llvm.maximum.f64(double %a, double %b)
  ret double %r
}
