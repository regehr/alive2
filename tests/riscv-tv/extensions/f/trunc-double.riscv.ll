define double @trunc_double(double noundef %0) {
  %2 = call double @llvm.trunc.f64(double %0)
  ret double %2
}
