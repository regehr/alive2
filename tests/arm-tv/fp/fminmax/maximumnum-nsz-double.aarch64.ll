; Default FPCR; symbolic inputs cover NaNs, infinities, and signed zeros.
define double @maximumnum_nsz_double(double noundef %a, double noundef %b) {
  %r = call nsz double @llvm.maximumnum.f64(double %a, double %b)
  ret double %r
}
