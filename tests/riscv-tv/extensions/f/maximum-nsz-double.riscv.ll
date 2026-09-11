; Permit either sign for mixed-sign zeros while retaining NaN handling.
define double @maximum_nsz_double(double noundef %a, double noundef %b) {
  %r = call nsz double @llvm.maximum.f64(double %a, double %b)
  ret double %r
}
