define dso_local noundef double @mind(double noundef %x, double noundef %y, double noundef %z) local_unnamed_addr #0 {
entry:
  %1 = call double @llvm.minnum.f64(double %x, double %y)
  %2 = call double @llvm.minnum.f64(double %z, double %1)
  ret double %2
}
