define double @rint_double(double noundef %0) {
  %2 = call double @llvm.rint.f64(double %0)
  ret double %2
}
