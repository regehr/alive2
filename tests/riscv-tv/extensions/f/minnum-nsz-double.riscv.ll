; Permit either sign for mixed-sign zeros while retaining NaN handling.
define double @minnum_nsz_double(double noundef %a, double noundef %b) {
  %r = call nsz double @llvm.minnum.f64(double %a, double %b)
  ret double %r
}
