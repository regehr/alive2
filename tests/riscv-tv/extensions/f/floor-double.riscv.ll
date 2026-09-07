define double @floor_double(double noundef %0) {
  %2 = call double @llvm.floor.f64(double %0)
  ret double %2
}
