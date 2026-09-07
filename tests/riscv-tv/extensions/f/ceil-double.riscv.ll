define double @ceil_double(double noundef %0) {
  %2 = call double @llvm.ceil.f64(double %0)
  ret double %2
}
