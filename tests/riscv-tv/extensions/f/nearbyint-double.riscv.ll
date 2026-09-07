define double @nearbyint_double(double noundef %0) {
  %2 = call double @llvm.nearbyint.f64(double %0)
  ret double %2
}
