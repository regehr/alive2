define double @round_double(double noundef %0) {
  %2 = call double @llvm.round.f64(double %0)
  ret double %2
}
