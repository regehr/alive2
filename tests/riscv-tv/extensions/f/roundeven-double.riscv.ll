define double @roundeven_double(double noundef %0) {
  %2 = call double @llvm.roundeven.f64(double %0)
  ret double %2
}
